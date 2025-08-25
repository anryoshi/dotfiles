vim9script

export def Memoize(F: func(): any): func(): any
  var cached: any = v:none
  return () => {
    # TODO: Figure out do we need the second condition
    #       and what the difference with is
    if type(cached) == v:t_none && cached == v:none
      cached = F()
    endif
    return cached
  }
enddef

export enum OS
  Unknown,
  Linux,
  Windows,
  macOS
endenum

export def DetermineOS(): OS
  if has("win64") || has("win32") || has("win16")
    return OS.Windows
  elseif has("macunix")
    return OS.macOS
  elseif has("unix")
    return OS.Linux
  else
    return OS.Unknown
  endif
enddef
export var GetOS = Memoize(DetermineOS)

export def DetermineStateDir(): string
  var os = DetermineOS()
  if os == OS.Windows
    return expand('~') .. '/vimfiles/state' 
  else
    return expand('~') .. '/.local/state/vim'
  endif
enddef
export var GetStateDir = Memoize(DetermineStateDir)

export def GitClone(url: string, path: string)
  if !executable('git')
    throw 'git not found'
  endif

  if isdirectory(path)
     # TODO: check that it's desired repo
     return
  endif

  EnsureDir(path)

  var cmd = printf('git clone %s %s', url, path)
  system(cmd)
  if v:shell_error != 0
    throw printf('git clone failed (exit %s)', v:shell_error)
  endif
enddef

export def EnsureDir(path: string)
  if !isdirectory(path)
    try
      mkdir(path, 'p')
    catch
      throw 'EnsureDir: failed to create directory "' .. path .. '"'
    endtry
  endif
enddef

export def CompareAndSetTimestampFile(path: string, delta: number): bool
  const now = localtime()

  def WriteOrThrow()
    try
      writefile([printf('%d', now)], path)
    catch
      throw 'CompareAndSetTimestampFile: failed to write "' .. path .. '"'
    endtry
  enddef

  if !filereadable(path)
    WriteOrThrow()
    return true
  endif

  var lines = readfile(path, '', 1)

  if empty(lines)
    WriteOrThrow()
    return true
  endif

  var line = lines[0]

  if match(line, '^\s*\d\+\s*$') != 0
    WriteOrThrow()
    return true
  endif

  var ts = str2nr(line, 10)

  if now - ts > delta
    WriteOrThrow()
    return true
  endif

  return false
enddef
