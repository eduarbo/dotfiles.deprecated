function data_diff()
    local in_seq = hs.execute(netspeed_in_str)
    local out_seq = hs.execute(netspeed_out_str)
    local in_diff = in_seq - netspeed_in_seq
    local out_diff = out_seq - netspeed_out_seq

    if in_diff/1024 > 1024 then
        kbin = string.format("%6.2f",in_diff/1024/1024) .. ' mb/s'
    else
        kbin = string.format("%6.2f",in_diff/1024) .. ' kb/s'
    end

    if out_diff/1024 > 1024 then
        kbout = string.format("%6.2f",out_diff/1024/1024) .. ' mb/s'
    else
        kbout = string.format("%6.2f",out_diff/1024) .. ' kb/s'
    end

    -- local disp_str = '⥄ '..kbout..'\n⥂ '..kbin
    local disp_str = '⬆'..kbout..'\n⬇'..kbin
    -- local disp_str = '⇧'..kbout..'\n⇩'..kbin
    -- local disp_str = '⇡'..kbout..'\n⇣'..kbin
    -- local disp_str = '↗'..kbout..'\n↙'..kbin

    if darkmode_status then
        styled_disp_str = hs.styledtext.new(disp_str,{font={size=9.0,color=white}})
    else
        styled_disp_str = hs.styledtext.new(disp_str,{font={size=9.0,color=black}})
    end

    netspeed_menubar:setTitle(styled_disp_str)
    netspeed_in_seq = in_seq
    netspeed_out_seq = out_seq
end

local darkmode_status = hs.osascript.applescript('tell application "System Events"\nreturn dark mode of appearance preferences\nend tell')
netspeed_active_interface = hs.network.primaryInterfaces()

if netspeed_active_interface ~= false then
    if not netspeed_menubar then
        netspeed_menubar = hs.menubar.new()
    end

    local interface_detail = hs.network.interfaceDetails(netspeed_active_interface)
    local menuitems_table = {}

    if interface_detail.AirPort then
        local ssid = interface_detail.AirPort.SSID
        table.insert(menuitems_table, {title="SSID: "..ssid, tooltip="Copy SSID to clipboard", fn=function() hs.pasteboard.setContents(ssid) end})
    end

    if interface_detail.IPv4 then
        local ipv4 = interface_detail.IPv4.Addresses[1]
        table.insert(menuitems_table, {title="IPv4: "..ipv4, tooltip="Copy IPv4 to clipboard", fn=function() hs.pasteboard.setContents(ipv4) end})
    end

    if interface_detail.IPv6 then
        local ipv6 = interface_detail.IPv6.Addresses[1]
        table.insert(menuitems_table, {title="IPv6: "..ipv6, tooltip="Copy IPv6 to clipboard", fn=function() hs.pasteboard.setContents(ipv6) end})
    end

    local macaddr = hs.execute('ifconfig '..netspeed_active_interface..' | grep ether | awk \'{print $2}\'')
    table.insert(menuitems_table, {title="MAC Addr: "..macaddr, tooltip="Copy MAC Address to clipboard", fn=function() hs.pasteboard.setContents(macaddr) end})
    netspeed_menubar:setMenu(menuitems_table)

    netspeed_in_str = 'netstat -ibn | grep -e ' .. netspeed_active_interface .. ' -m 1 | awk \'{print $7}\''
    netspeed_out_str = 'netstat -ibn | grep -e ' .. netspeed_active_interface .. ' -m 1 | awk \'{print $10}\''

    netspeed_in_seq = hs.execute(netspeed_in_str)
    netspeed_out_seq = hs.execute(netspeed_out_str)

    if nettimer == nil then
        nettimer = hs.timer.doEvery(1,data_diff)
    else
        nettimer:start()
    end
end
