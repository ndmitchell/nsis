
-- | Windows 7 Taskbar Progress plugin: <http://nsis.sourceforge.net/TaskbarProgress_plug-in>
module Development.NSIS.Plugins.Taskbar(taskbar) where

import Development.NSIS


-- | Enable Windows 7 taskbar plugin, called anywhere.
taskbar :: Action ()
taskbar = onPageShow InstFiles $ plugin "w7tbp" "Start" []
