local build_dir = path.join(os.curdir(), "build")
local scr_dir = path.join(os.curdir(), "src", "test", "lua")
local ut_scr_dir = path.join(scr_dir, "unitest")
local env_scr_dir = path.join(scr_dir, "common")

target("TestTLUL2ChiBridge")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        path.join(build_dir, "TLUL2ChiBridge", "*.sv"),
        path.join(env_scr_dir, "*.lua")
    )

    set_values("cfg.top", "TLUL2ChiBridge")
    set_values("cfg.lua_main", path.join(ut_scr_dir, "TLUL2ChiBridge.lua"))
    set_values("cfg.build_dir_name", "TestTLUL2ChiBridge")

target("TestEb")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        path.join(build_dir, "Eb", "*.sv"),
        path.join(env_scr_dir, "*.lua")
    )

    set_values("cfg.top", "EjectBufferSNP")
    set_values("cfg.lua_main", path.join(ut_scr_dir, "EjectBuffer.lua"))
    set_values("cfg.build_dir_name", "TestEb")

target("TestSct")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        path.join(build_dir, "Sct", "*.sv"),
        path.join(env_scr_dir, "*.lua")
    )

    set_values("cfg.top", "SingleChannelTapLocalSNP")
    set_values("cfg.lua_main", path.join(ut_scr_dir, "SingleChannelTap.lua"))
    set_values("cfg.build_dir_name", "TestSct")

target("TestClb")
    add_rules("verilua")
    add_toolchains("@vcs")

    add_files(
        path.join(build_dir, "Clb", "*.sv"),
        path.join(env_scr_dir, "*.lua")
    )

    set_values("cfg.top", "C2cLoopBack")
    set_values("cfg.lua_main", path.join(ut_scr_dir, "C2cLoopBack.lua"))
    set_values("cfg.build_dir_name", "TestClb")

task("verdi", function ()
  set_menu {
    usage = "xmake verdi [options]",
    description = "Display waveform with verdi",
    options = {
      {'c', "case", "kv", nil, "case name"},
    }
  }

  on_run(function ()
    import("core.base.option")
    assert(option.get("case"))
    local sim_dir = path.join(os.curdir(), "build", "vcs")
    local case = option.get("case")
    sim_dir = path.join(sim_dir, option.get("case"))

    os.cd(sim_dir)
    local cmds = "verdi -ssf test.vcd.fsdb -nologo"
    if os.exists("verdi.sh") then os.rm("verdi.sh") end
    io.writefile("verdi.sh", cmds)
    print(cmds)
    os.execv(os.shell(), { "verdi.sh" })
    os.rm("verdi.sh")
    end)
end)