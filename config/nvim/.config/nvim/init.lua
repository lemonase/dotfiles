-- init.lua --

-- general vim settings
require('config.settings')          -- general vim settings
require('config.keymaps')           -- general vim keymaps
require('config.platform_specific') -- platform specific settings (Win32, Mac, Linux)

-- plugin configurations
require('config.lazy')              -- nvim plugins
require('plugin-config')            -- plugin specific configurations

-- local config
require('config.local')
