`timescale 1ns/1ns

`include "uvm_macros.svh"
import uvm_pkg::*;

// ================= transaction =================
class transaction extends uvm_sequence_item;
  randc logic [3:0] a, b;
  randc logic [2:0] opsel;
  logic [15:0] result;

  function new(string name = "transaction"); super.new(name); endfunction

  `uvm_object_utils_begin(transaction)
    `uvm_field_int(a,      UVM_ALL_ON)
    `uvm_field_int(b,      UVM_ALL_ON)
    `uvm_field_int(opsel,  UVM_ALL_ON)
    `uvm_field_int(result, UVM_ALL_ON)
  `uvm_object_utils_end
endclass

// ================= functional coverage =================
class coverage extends uvm_subscriber #(transaction);
  `uvm_component_utils(coverage)
  
  transaction t;
  int cross_hits;
  
  covergroup alu_cg;
    // Coverpoint for a: 16 bins (0..15)
    a_cp: coverpoint t.a {
      bins a_vals[] = {[0:15]};
    }

    // Coverpoint for b: 16 bins (0..15)
    b_cp: coverpoint t.b {
      bins b_vals[] = {[0:15]};
    }

    // Coverpoint for opcode: 8 bins (0..7)
    opcode_cp: coverpoint t.opsel {
      bins op_vals[] = {[0:7]};
    }

    // Full cross: 16 x 16 x 8 = 2048 bins
    abc_cross: cross a_cp, b_cp, opcode_cp;
  endgroup


  function new(string name = "coverage", uvm_component parent = null);
    super.new(name, parent);
    alu_cg = new();
  endfunction

  virtual function void write(transaction t);
    this.t = t;
    alu_cg.sample();
    cross_hits++; // Each sample is a cross hit since we have only one bin each
  endfunction
  
  virtual function void report_phase(uvm_phase phase);
    super.report_phase(phase);
    $display("=== FUNCTIONAL COVERAGE REPORT ===");
    $display("Overall Coverage: %.2f%%", alu_cg.get_coverage());
    $display("A Coverage: %.2f%%", alu_cg.a_cp.get_coverage());
    $display("B Coverage: %.2f%%", alu_cg.b_cp.get_coverage());
    $display("Opcode Coverage: %.2f%%", alu_cg.opcode_cp.get_coverage());
    $display("A x B x Opcode Cross Coverage: %.2f%%", alu_cg.abc_cross.get_coverage());
    $display("");
    $display("=== CROSS COVERAGE HITS ===");
    $display("Total Cross Hits: %0d", cross_hits);
    $display("Cross Bin: A[all] x B[all] x Opcode[all] = %0d hits", cross_hits);
    $display("===========================");
  endfunction
endclass

// ================= generator (sequence) =================
class generator extends uvm_sequence #(transaction);
  transaction t;
  function new(string name = "generator"); super.new(name); endfunction
  `uvm_object_utils(generator)

  virtual task body();
    // Loop through all 16 x 16 x 8 = 2048 combinations
    for (int a_val = 0; a_val < 16; a_val++) begin
      for (int b_val = 0; b_val < 16; b_val++) begin
        for (int op_val = 0; op_val < 8; op_val++) begin
          t = transaction::type_id::create("t");
          start_item(t);
          t.a     = a_val;
          t.b     = b_val;
          t.opsel = op_val;
       `uvm_info("GEN", $sformatf("Deterministic Data -> a:%0d b:%0d opsel:%0d",
                                      t.a, t.b, t.opsel), UVM_LOW)
          finish_item(t);
        end
      end
    end
  endtask
endclass


// ================= driver =================
class driver extends uvm_driver #(transaction);
  `uvm_component_utils(driver)
  virtual alu_vif aif;
  transaction req; 

  function new(string name = "driver", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual alu_vif)::get(this, "", "aif", aif))
      `uvm_fatal("DRV", "Unable to access virtual interface from uvm_config_db")
  endfunction

  virtual task run_phase(uvm_phase phase);
    forever begin
      seq_item_port.get_next_item(req);
      @(posedge aif.clk);
      aif.a <= req.a;
      aif.b <= req.b;
      aif.opsel <= req.opsel;
      `uvm_info("DRV", $sformatf("Trigger to DUT a:%0d b:%0d opsel:%0d", req.a, req.b, req.opsel), UVM_LOW)
      seq_item_port.item_done();
    end
  endtask
endclass

// ================= monitor =================
class monitor extends uvm_monitor;
  `uvm_component_utils(monitor)
  uvm_analysis_port #(transaction) send;
  virtual alu_vif aif;
  transaction t;
 
  function new(string name = "monitor", uvm_component parent = null);
    super.new(name, parent);
    send = new("send", this);
  endfunction

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    if (!uvm_config_db#(virtual alu_vif)::get(this, "", "aif", aif))
      `uvm_fatal("MON", "Unable to access virtual interface from uvm_config_db")
  endfunction

  virtual task run_phase(uvm_phase phase);
    
    logic [3:0] prev_a, prev_b;
    logic [2:0] prev_opsel;
    bit first_cycle = 1;
    
    forever begin
      @(posedge aif.clk);
      #2; //settling time
      
      if (!first_cycle) begin
        
        t = transaction::type_id::create("t");
        t.a      = prev_a;     
        t.b      = prev_b;      
        t.opsel  = prev_opsel;  
        t.result = aif.result; 
        
        `uvm_info("MON", $sformatf("To scoreboard a:%0d b:%0d opsel:%0d result:%0d",
                 t.a, t.b, t.opsel, t.result), UVM_LOW)
        send.write(t);
      end
      else begin
        first_cycle = 0;
      end
      
      prev_a     = aif.a;
      prev_b     = aif.b;
      prev_opsel = aif.opsel;
    end
  endtask
endclass

// ================= scoreboard =================
class scoreboard extends uvm_scoreboard;
  `uvm_component_utils(scoreboard)
  uvm_analysis_imp #(transaction, scoreboard) recv;

  function new(string name = "scoreboard", uvm_component parent = null);
    super.new(name, parent);
    recv = new("recv", this);
  endfunction

  virtual function void write(transaction t);
    bit [15:0] expected;
    case (t.opsel)
      3'b000: expected = t.a + t.b;
      3'b001: expected = t.a * t.b;
      3'b010: expected = (t.b != 0) ? t.a / t.b : 16'hx;
      3'b011: expected = (t.b != 0) ? t.a % t.b : 16'hx;
      3'b100: expected = t.a ^ t.b;
      3'b101: expected = t.a | t.b;
      3'b110: expected = t.a & t.b;
      3'b111: expected = t.a << t.b;
      default: expected = t.a + t.b;
    endcase
    
    if (t.b == 0 && (t.opsel == 3'b010 || t.opsel == 3'b011)) begin
      if (t.result === 16'hx)
        `uvm_info("SCOREBOARD", $sformatf("PASS (Div by 0) a=%0d b=%0d opsel=%0d result=x",
                    t.a, t.b, t.opsel), UVM_LOW)
      else
        `uvm_error("SCOREBOARD", $sformatf("MISMATCH (Div by 0) a=%0d b=%0d opsel=%0d DUT=%0d Expected=x",
                    t.a, t.b, t.opsel, t.result))
    end
    else if (t.result !== expected) begin
      `uvm_error("SCOREBOARD", $sformatf("MISMATCH a=%0d b=%0d opsel=%0d DUT=%0d Expected=%0d",
                  t.a, t.b, t.opsel, t.result, expected))
    end
    else begin
      `uvm_info("SCOREBOARD", $sformatf("PASS a=%0d b=%0d opsel=%0d result=%0d",
                 t.a, t.b, t.opsel, t.result), UVM_LOW)
    end
  endfunction
endclass

// ================= agent/env =================
class agent extends uvm_agent;
  `uvm_component_utils(agent)
  monitor m;
  driver d;
  uvm_sequencer #(transaction) seqr;

  function new(string name = "agent", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    m = monitor::type_id::create("m", this);
    d = driver ::type_id::create("d", this);
    seqr = uvm_sequencer#(transaction)::type_id::create("seqr", this);
  endfunction

  virtual function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    d.seq_item_port.connect(seqr.seq_item_export);
  endfunction
endclass

class env extends uvm_env;
  `uvm_component_utils(env)
  scoreboard s;
  coverage cov;
  agent a;

  function new(string name = "env", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    a = agent::type_id::create("a", this);
    s = scoreboard::type_id::create("s", this);
    cov = coverage::type_id::create("cov", this);
  endfunction

  virtual function void connect_phase(uvm_phase phase);
    super.connect_phase(phase);
    a.m.send.connect(s.recv);
    a.m.send.connect(cov.analysis_export);
  endfunction
endclass

// ================= test =================
class test extends uvm_test;
  `uvm_component_utils(test)
  generator gen;
  env e;

  function new(string name = "test", uvm_component parent = null);
    super.new(name, parent);
  endfunction

  virtual function void build_phase(uvm_phase phase);
    super.build_phase(phase);
    e = env::type_id::create("e", this);
  endfunction

  virtual task run_phase(uvm_phase phase);
    phase.raise_objection(this);
    gen = generator::type_id::create("gen");
    gen.start(e.a.seqr);
    #100; 
    phase.drop_objection(this);
  endtask
endclass

module alu_tb;
  logic clk;
  logic rst;

  alu_vif aif(clk, rst);
  alu dut(.clk(clk), .rst(rst),
          .a(aif.a), .b(aif.b), .opsel(aif.opsel), .result(aif.result));

  initial begin
    clk = 0;
    forever #5 clk = ~clk;
  end

  initial begin
    $dumpfile("dump.vcd");
    $dumpvars;
  end

  initial begin
    uvm_config_db#(virtual alu_vif)::set(null, "uvm_test_top.e.a.*", "aif", aif);
    run_test("test");
  end
endmodule

      
    

