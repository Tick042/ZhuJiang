local env = require "env"
local chi = require "CHI"
local Queue = require "Queue"

local flit_num = 100000
local print_intvl = flit_num / 20
local max_deq_rdy = 8
local time_out = 0

local flit = [[
    | valid
    | bits
    | ready
    ]]

local enq_req = (flit):bundle {hier = cfg.top, prefix = "io_enq_req_", is_decoupled = false}
local enq_rsp = (flit):bundle {hier = cfg.top, prefix = "io_enq_rsp_", is_decoupled = false}
local enq_dat = (flit):bundle {hier = cfg.top, prefix = "io_enq_dat_", is_decoupled = false}
local enq_snp = (flit):bundle {hier = cfg.top, prefix = "io_enq_snp_", is_decoupled = false}

local deq_req = (flit):bundle {hier = cfg.top, prefix = "io_deq_req_", is_decoupled = false}
local deq_rsp = (flit):bundle {hier = cfg.top, prefix = "io_deq_rsp_", is_decoupled = false}
local deq_dat = (flit):bundle {hier = cfg.top, prefix = "io_deq_dat_", is_decoupled = false}
local deq_snp = (flit):bundle {hier = cfg.top, prefix = "io_deq_snp_", is_decoupled = false}

local req_q = Queue()
local rsp_q = Queue()
local dat_q = Queue()
local snp_q = Queue()

local function shuffle(this)
    local u32_vec_size = (this.bits.width + 31) / 32
    local val_table = {}
    for i = 1, u32_vec_size do val_table[i] = urandom() end
    this.bits:set(val_table)
end

local function fire(this)
    return this.valid:is(1) and this.ready:is(1)
end

enq_req.shuffle = shuffle
enq_rsp.shuffle = shuffle
enq_dat.shuffle = shuffle
enq_snp.shuffle = shuffle

deq_req.shuffle = shuffle
deq_rsp.shuffle = shuffle
deq_dat.shuffle = shuffle
deq_snp.shuffle = shuffle

enq_req.fire = fire
enq_rsp.fire = fire
enq_dat.fire = fire
enq_snp.fire = fire

deq_req.fire = fire
deq_rsp.fire = fire
deq_dat.fire = fire
deq_snp.fire = fire

local function inject()
    local cnt = 0
    local cnt_by_interval = 0
    local req_cnt = 0
    local rsp_cnt = 0
    local dat_cnt = 0
    local snp_cnt = 0
    local req_fire = false
    local rsp_fire = false
    local dat_fire = false
    local snp_fire = false
    enq_req.valid:set(1)
    enq_rsp.valid:set(1)
    enq_dat.valid:set(1)
    enq_snp.valid:set(1)
    enq_req:shuffle()
    enq_rsp:shuffle()
    enq_dat:shuffle()
    enq_snp:shuffle()
    repeat
        env.posedge()
        if(enq_req.ready:is(1)) then
            req_q:push({req_cnt, enq_req.bits:get_hex_str()})
            req_fire = true
            req_cnt = req_cnt + 1
        end
        if(enq_rsp.ready:is(1)) then
            rsp_q:push({rsp_cnt, enq_rsp.bits:get_hex_str()})
            rsp_fire = true
            rsp_cnt = rsp_cnt + 1
        end
        if(enq_dat.ready:is(1)) then
            dat_q:push({dat_cnt, enq_dat.bits:get_hex_str()})
            dat_fire = true
            dat_cnt = dat_cnt + 1
        end
        if(enq_snp.ready:is(1)) then
            snp_q:push({snp_cnt, enq_snp.bits:get_hex_str()})
            snp_fire = true
            snp_cnt = snp_cnt + 1
        end
        cnt = req_cnt + rsp_cnt + dat_cnt + snp_cnt
        if(cnt_by_interval ~= math.floor(cnt / print_intvl) or cnt >= flit_num) then
            cnt_by_interval = math.floor(cnt / print_intvl)
            printf("%g flits have been enqueued ...\n", cnt);
        end
        env.negedge()
        if(req_fire) then
            enq_req:shuffle()
            req_fire = false
        end
        if(rsp_fire) then
            enq_rsp:shuffle()
            rsp_fire = false
        end
        if(dat_fire) then
            enq_dat:shuffle()
            dat_fire = false
        end
        if(snp_fire) then
            enq_snp:shuffle()
            snp_fire = false
        end
    until (cnt >= flit_num)
    enq_req.valid:set(0)
    enq_rsp.valid:set(0)
    enq_dat.valid:set(0)
    enq_snp.valid:set(0)
end

local function eject_rdy_gen()
    local req_rdy_cnt = urandom_range(1, max_deq_rdy)
    local rsp_rdy_cnt = urandom_range(1, max_deq_rdy)
    local dat_rdy_cnt = urandom_range(1, max_deq_rdy)
    local snp_rdy_cnt = urandom_range(1, max_deq_rdy)
    deq_req.ready:set(0)
    deq_rsp.ready:set(0)
    deq_dat.ready:set(0)
    deq_snp.ready:set(0)
    repeat
        env.negedge()
        if(req_rdy_cnt == 0) then
            deq_req.ready:set(1)
        else
            deq_req.ready:set(0)
            req_rdy_cnt = req_rdy_cnt - 1
        end
        if(rsp_rdy_cnt == 0) then
            deq_rsp.ready:set(1)
        else
            deq_rsp.ready:set(0)
            rsp_rdy_cnt = rsp_rdy_cnt - 1
        end
        if(dat_rdy_cnt == 0) then
            deq_dat.ready:set(1)
        else
            deq_dat.ready:set(0)
            dat_rdy_cnt = dat_rdy_cnt - 1
        end
        if(snp_rdy_cnt == 0) then
            deq_snp.ready:set(1)
        else
            deq_snp.ready:set(0)
            snp_rdy_cnt = snp_rdy_cnt - 1
        end
        env.posedge()
        if deq_req:fire() then
            req_rdy_cnt = urandom_range(1, max_deq_rdy)
        end
        if deq_rsp:fire() then
            rsp_rdy_cnt = urandom_range(1, max_deq_rdy)
        end
        if deq_dat:fire() then
            dat_rdy_cnt = urandom_range(1, max_deq_rdy)
        end
        if deq_snp:fire() then
            snp_rdy_cnt = urandom_range(1, max_deq_rdy)
        end

    until false
end

local function eject_dat_chk()
    local cnt = 0
    local cnt_by_interval = 0
    local req_cnt = 0
    local rsp_cnt = 0
    local dat_cnt = 0
    local snp_cnt = 0
    repeat
        env.posedge()
        if deq_req:fire() then
            deq_req.bits:expect_hex_str(req_q:front()[2])
            req_q:pop()
            req_cnt = req_cnt + 1
        end
        if deq_rsp:fire() then
            deq_rsp.bits:expect_hex_str(rsp_q:front()[2])
            rsp_q:pop()
            rsp_cnt = rsp_cnt + 1
        end
        if deq_dat:fire() then
            deq_dat.bits:expect_hex_str(dat_q:front()[2])
            dat_q:pop()
            dat_cnt = dat_cnt + 1
        end
        if deq_snp:fire() then
            deq_snp.bits:expect_hex_str(snp_q:front()[2])
            snp_q:pop()
            snp_cnt = snp_cnt + 1
        end
        cnt = req_cnt + rsp_cnt + dat_cnt + snp_cnt
        if(cnt_by_interval ~= math.floor(cnt / print_intvl) or cnt >= flit_num) then
            cnt_by_interval = math.floor(cnt / print_intvl)
            printf("%g flits have been dequeued ...\n", cnt);
        end
    until (cnt >= flit_num)
end

local function time_out_chk()
    local timer = 0
    if time_out ~= 0 then
        repeat
            env.posedge()
            timer = timer + 1
            if timer > time_out then
                assert(false)
            end
        until false
    end
end

local test_main = env.register_test_case "test_main" {
    function ()
        env.negedge()
        dut.io_p0UserIn:chdl():set({0x0123, 0x4567, 0x89ab})
        dut.io_p1UserIn:chdl():set({0xba98, 0x7654, 0x3210})
        local str0 = dut.io_p0UserIn:get_hex_str()
        local str1 = dut.io_p1UserIn:get_hex_str()
        env.dut_reset()
        env.negedge(10)
        repeat
            env.posedge()
        until dut.io_p0UserOut_valid:is(1) and dut.io_p1UserOut_valid:is(1)
        dut.io_p0UserOut_bits:expect_hex_str(str1)
        dut.io_p1UserOut_bits:expect_hex_str(str0)
        env.negedge()
        fork {
            function ()
                eject_rdy_gen()
            end,
            function ()
                inject()
            end,
            function ()
                time_out_chk()
            end
        }
        eject_dat_chk()
    end
}

fork {
    function ()
        sim.dump_wave()
        test_main()
        env.negedge(5)
        env.TEST_SUCCESS()
    end
}