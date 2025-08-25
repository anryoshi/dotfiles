vim9script

import autoload "kiso.vim"

var plugins: list<list<any>> = []

def Bootstrap()
  var minpac_url = 'https://github.com/k-takata/minpac'
  var minpac_dir = split(&packpath, ',')[0] .. '/pack/minpac/opt/minpac'
  kiso.GitClone(minpac_url, minpac_dir)
enddef

def Outdated(): bool
  kiso.EnsureDir(kiso.GetStateDir())
  return kiso.CompareAndSetTimestampFile(kiso.GetStateDir() .. '/minpac_timestamp', 60 * 60 * 24)
enddef

def Init()
  packadd minpac

  minpac#init()

  for [url, opts] in plugins
    minpac#add(url, opts)
  endfor
enddef

def AddPlugin(repo: string, opts: dict<any> = {})
  add(plugins, [repo, opts])
enddef

def RegisterCommands()
  command! -bar -nargs=+ Pack AddPlugin(<args>)
  command! -bar HakoUpdate Init() | minpac#update()
enddef

export def Begin()
  Bootstrap()
  RegisterCommands()
enddef

export def End()
  delc Pack

  if Outdated()
    HakoUpdate
  endif
enddef


