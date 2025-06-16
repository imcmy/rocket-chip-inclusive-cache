/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

class FlushAllRequest(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val invalidate = Bool()
}

class FlushAllController(params: InclusiveCacheParameters) extends Module
{
  val io = IO(new Bundle {
    // Control interface
    val start = Flipped(Decoupled(new FlushAllRequest(params)))
    val done = Valid(Bool())
    
    // Directory interface
    val dir_read = Valid(new DirectoryRead(params))
    val dir_result = Flipped(Valid(new DirectoryResult(params)))
    
    // Request generation interface
    val flush_req = Decoupled(new FullRequest(params))
    val flush_resp = Flipped(Valid(Bool()))
  })

  // State machine states
  val s_idle :: s_read_dir :: s_wait_dir :: s_issue_flush :: s_wait_flush :: s_done :: Nil = Enum(6)
  val state = RegInit(s_idle)
  
  // Counters for iterating through cache
  val set_counter = RegInit(0.U(params.setBits.W))
  val way_counter = RegInit(0.U(params.wayBits.W))
  val invalidate_mode = RegInit(false.B)
  
  // Progress tracking
  val last_set = (params.cache.sets - 1).U
  val last_way = (params.cache.ways - 1).U
  val is_last_way = way_counter === last_way
  val is_last_set = set_counter === last_set
  val is_complete = is_last_set && is_last_way
  
  // Default outputs
  io.start.ready := state === s_idle
  io.done.valid := state === s_done
  io.done.bits := true.B
  io.dir_read.valid := state === s_read_dir
  io.dir_read.bits.set := set_counter
  io.dir_read.bits.tag := 0.U // Don't care for directory scan
  io.flush_req.valid := false.B
  io.flush_req.bits := DontCare
  
  // State machine
  switch (state) {
    is (s_idle) {
      when (io.start.valid) {
        invalidate_mode := io.start.bits.invalidate
        set_counter := 0.U
        way_counter := 0.U
        state := s_read_dir
      }
    }
    
    is (s_read_dir) {
      // Issue directory read for current set
      state := s_wait_dir
    }
    
    is (s_wait_dir) {
      when (io.dir_result.valid) {
        // Check if this way is valid and needs flushing
        val ways_data = io.dir_result.bits.asTypeOf(Vec(params.cache.ways, new DirectoryEntry(params)))
        val current_way_data = ways_data(way_counter)
        val needs_flush = current_way_data.state =/= MetaData.INVALID
        
        when (needs_flush) {
          state := s_issue_flush
        } .otherwise {
          // Move to next way/set
          when (is_complete) {
            state := s_done
          } .elsewhen (is_last_way) {
            way_counter := 0.U
            set_counter := set_counter + 1.U
            state := s_read_dir
          } .otherwise {
            way_counter := way_counter + 1.U
            state := s_read_dir
          }
        }
      }
    }
    
    is (s_issue_flush) {
      io.flush_req.valid := true.B
      io.flush_req.bits.prio := VecInit(1.U(3.W).asBools) // Same priority as channel A
      io.flush_req.bits.control.flush := true.B
      io.flush_req.bits.control.invalidate := invalidate_mode
      io.flush_req.bits.opcode := 0.U
      io.flush_req.bits.param := 0.U
      io.flush_req.bits.size := params.offsetBits.U
      io.flush_req.bits.source := params.inner.client.clients.map(_.sourceId.start).min.U
      io.flush_req.bits.offset := 0.U
      io.flush_req.bits.set := set_counter
      
      // Get tag from directory result
      val ways_data = io.dir_result.bits.asTypeOf(Vec(params.cache.ways, new DirectoryEntry(params)))
      io.flush_req.bits.tag := ways_data(way_counter).tag
      io.flush_req.bits.put := 0.U
      
      when (io.flush_req.ready) {
        state := s_wait_flush
      }
    }
    
    is (s_wait_flush) {
      when (io.flush_resp.valid) {
        // Move to next way/set
        when (is_complete) {
          state := s_done
        } .elsewhen (is_last_way) {
          way_counter := 0.U
          set_counter := set_counter + 1.U
          state := s_read_dir
        } .otherwise {
          way_counter := way_counter + 1.U
          state := s_read_dir
        }
      }
    }
    
    is (s_done) {
      when (io.done.ready) {
        state := s_idle
      }
    }
  }
  
  // Coverage and assertions
  params.ccover(state === s_idle && io.start.valid, "FLUSHALL_START", "FlushAll operation started")
  params.ccover(state === s_done, "FLUSHALL_DONE", "FlushAll operation completed")
  params.ccover(state === s_issue_flush && io.flush_req.valid, "FLUSHALL_FLUSH_REQ", "Issued flush request for valid cache line")
  
  assert(!(state === s_issue_flush && !io.dir_result.valid), "Directory result must be valid when issuing flush")
  assert(!(way_counter >= params.cache.ways.U), "Way counter overflow")
  assert(!(set_counter >= params.cache.sets.U), "Set counter overflow")
}
