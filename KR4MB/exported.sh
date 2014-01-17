#!/bin/sh

# Command to export settings:
# /Applications/KeyRemap4MacBook.app/Contents/Applications/KeyRemap4MacBook_cli.app/Contents/MacOS/KeyRemap4MacBook_cli export > exported.sh 

cli=/Applications/KeyRemap4MacBook.app/Contents/Applications/KeyRemap4MacBook_cli.app/Contents/MacOS/KeyRemap4MacBook_cli

$cli set remap.doublepresscommandQ 1
/bin/echo -n .
$cli set parameter.simultaneouskeypresses_delay 10
/bin/echo -n .
$cli set repeat.consumer_initial_wait 300
/bin/echo -n .
$cli set parameter.simultaneouskeypresses_pointingbutton_delay 10
/bin/echo -n .
$cli set remap.pc_application2commandL 1
/bin/echo -n .
$cli set option.emacsmode_controlW 1
/bin/echo -n .
$cli set repeat.wait 20
/bin/echo -n .
$cli set option.emacsmode_controlSlash 1
/bin/echo -n .
$cli set option.emacsmode_ex_controlSpace 1
/bin/echo -n .
$cli set remap.reverse_vertical_scrolling 1
/bin/echo -n .
$cli set parameter.holdingkeytokey_wait 100
/bin/echo -n .
$cli set remap.option_brackets_to_command_shift_brackets 1
/bin/echo -n .
$cli set parameter.keyoverlaidmodifier_initial_modifier_wait 10
/bin/echo -n .
$cli set option.vimode_commandl_hjkl 1
/bin/echo -n .
$cli set remap.space2controlL_space 1
/bin/echo -n .
$cli set option.emacsmode_controlY 1
/bin/echo -n .
$cli set option.emacsmode_OptionWCopy 1
/bin/echo -n .
$cli set repeat.initial_wait 300
/bin/echo -n .
/bin/echo
