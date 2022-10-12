local notify = require('notify')

local Spinner = {}
Spinner.__index = Spinner
setmetatable(Spinner, {
  __call = function(cls, ...)
    return cls.new(...)
  end,
})

local spinner_frames = { '⣾', '⣽', '⣻', '⢿', '⡿', '⣟', '⣯', '⣷' }

function Spinner.new(msg, lvl, opts)
  local self = setmetatable({}, Spinner)

  self.msg = msg
  self.lvl = lvl
  self.opts = opts or {}
  self.opts.timeout = false

  self:_spin()

  return self
end

function Spinner:_update(msg, lvl, opts)
  -- TODO: debounce
  opts = opts or {}
  opts.replace = self.id
  opts.hide_from_history = true
  self.id = notify(msg, lvl, opts)
end

function Spinner:update(msg, lvl, opts)
  if msg ~= nil then
    self.msg = msg
  end
  if lvl ~= nil then
    self.lvl = lvl
  end
  if opts ~= nil then
    self.opts = opts
  end
end

function Spinner:_spin()
  if self.timer then
    if self.timer:is_closing() then
      return
    end
    self.timer:close()
  end

  local opts = self.opts or {}
  if opts.icon == nil then
    self.frame = (self.frame or 0) % #spinner_frames + 1
    opts.icon = spinner_frames[self.frame]
  end
  self:_update(self.msg, self.lvl, opts)
  self.msg = nil
  self.lvl = nil
  self.opts = nil

  self.timer = vim.loop.new_timer()
  self.timer:start(1000/#spinner_frames, 0, vim.schedule_wrap(function()
    self:_spin()
  end))
end

function Spinner:done(msg, lvl, opts)
  if not self.timer:is_closing() then
    self.timer:close()
  end

  opts = opts or {}
  if opts.timeout == nil then
    opts.timeout = 3000
  end

  self:_update(msg, lvl, opts)
end

return Spinner
