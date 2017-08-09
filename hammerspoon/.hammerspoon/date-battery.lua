-- Battery
local previousPowerSource = hs.battery.powerSource()

local function showDateTime()
    local date = os.date('%A, %h %e')
    local time = os.date('%I:%M%p'):gsub('^0',''):lower()
    return '🕒 '..time..'   📆 '..date
end

function minutesToHours(minutes)
    if minutes <= 0 then
        return "0:00";
    else
        hours = string.format("%d", math.floor(minutes / 60))
        mins = string.format("%02.f", math.floor(minutes - (hours * 60)))
        return string.format("%s:%s", hours, mins)
    end
end

function batteryStatus()
    local message = "🔋"
    local pct = hs.battery.percentage()

    if hs.battery.isCharging() then
        local untilFull = hs.battery.timeToFullCharge()
        message = message .. " Charging:"

        if untilFull == -1 then
            message = string.format("%s %.0f%% (calculating...)", message, pct);
        else
            local watts = hs.battery.watts()
            message = string.format("%s %.0f%% (%s remaining @ %.1fW)", message, pct, minutesToHours(untilFull), watts)
        end
    elseif hs.battery.powerSource() == "Battery Power" then
        local untilEmpty = hs.battery.timeRemaining()

        if untilEmpty == -1 then
            message = string.format("%s %.0f%% (calculating...)", message, pct)
        else
            local watts = hs.battery.watts()
            message = string.format("%s %.0f%% (%s remaining @ %.1fW)", message, pct, minutesToHours(untilEmpty), watts)
        end
    else
        message = message .. " Fully charged"
    end

    return message
end

function showStatus()
    local message = showDateTime() .. "\n" .. batteryStatus()

    hs.alert.closeAll()
    hs.alert.show(message)
end

function batteryChangedCallback()
    local powerSource = hs.battery.powerSource()

    if powerSource ~= previousPowerSource then
        showStatus()
        previousPowerSource = powerSource;
    end
end

hs.battery.watcher.new(batteryChangedCallback):start()

hs.hotkey.bind('rightalt', ".", showStatus)
