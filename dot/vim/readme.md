# Thoughts about Vim

## Intro
[_Vim_](https://www.vim.org) (or its modern alternative [_NeoVim_](https://neovim.io)) is great but scary for most users.

### Why is it great? :
* __modal nature__: combining text objects and operations boosts productivity
* __extensibility__: `Vimscript` (with asynchronous jobs and channels in Vim8), and API for most popular scripting languages (`Lua`, `Python`, `Ruby`)
* __it is vi__: because _Vim_ stands for _Vi iMproved_ it is available almost in every modern Unix-like OS environment and can be efficient out of the box

### Why is it scary? :
* __modal nature__: it is not intuitive for users with regular experience
* __extensibility__: it forces user to dive into `Vimscript` to feel comfort with plugins and etc, although the good news is
  > __Vimscript__ is _Vim_
  > \- [__Steve Losh__](http://stevelosh.com)
* __it is vi__: the core idea of _Vim_ comes from 1970th and makes Vim the first-class citizen of the terminal world. Although GUI version, the way they exposed their functionality not standardized between implementations and platforms

### Where to start? :
1. __vimtutor__ - is a best place to start with vim because it ships with almost every vim distribution.

    Just type `vimtutor` and read through text following commands.

    Other fun but a bit messy option is [Vim Adventures](https://vim-adventures.com)

1. [Practical Vim](https://pragprog.com/book/dnvim2/practical-vim-second-edition) - after getting basics in step one it is possible to dive into "How to use _Vim_ effectively?" topic.

1. [Learn Vimscript the Hard Way](http://learnvimscriptthehardway.stevelosh.com) - the last part is to learn about `Vimscript` and its incorporation in _Vim_ itself.


## Vim Configuration
This section mostly describes content of my ___vimrc___ (_Vim_ [run commands](https://en.wikipedia.org/wiki/Run_commands)) file

### Plugin manager
Although _Vim_ has great community that contributes in creation of dozens plugins there is no standard way to manage extensions (or plugins, or packages whatever you prefer) to default _Vim_ functionality like [Emacs Packages](https://www.gnu.org/software/emacs/manual/html_node/emacs/Packages.html)

_Vim_ acts in term of `runtimepath` where you have location populated you by hand with different `Vimscript` files

There are a lot of different plugin managers developed mostly on GitHub but i prefer:
* ~~[__pathogen.vim__](https://github.com/tpope/vim-pathogen)~~ - allows you to store plugin files in separate folders under one adding to runtime all of them with one command

  __update__: functionality similar to __pathogen.vim__ available in _Vim8_ [out of box](https://vimhelp.org/repeat.txt.html#packages)
  
* [__vim-plug__](https://github.com/junegunn/vim-plug) - comparing with __pathogen.vim__ provides functionality to specify all your plugins inside the ___vimrc___ file itself and to perform asynchronous download of them 

### Organization
In ___vimrc___ I tend to extensive usage of [folding](https://vim.fandom.com/wiki/Folding) splitting file into sections:
1. Setting _Vim_ default settings coming from Vim 8 (because non default ___vimrc___ does not source it)
2. Automatically install __vim-plug__ if it is not installed
3. Specifies list of plugins to download from __GitHub__
4. Setting up basic settings not related to plugins
5. Settings related to plugins
6. Temporary settings that are not in 4 or 5 yet
6. Sourcing local ___vimrc___ file containing host specific settings

### Basic configuration

### External Plugins

* Great set of plugins created by [__Tim Pope__](https://tpo.pe)
  * [__vim-sensible__](https://github.com/tpope/vim-sensible)
  * [__vim-surround__](https://github.com/tpope/vim-surround)
  * [__vim-commentary__](https://github.com/tpope/tpope/vim-commentary)
  * [__vim-sleuth__](https://github.com/tpope/vim-sleuth)
  * [__vim-speeddating__](https://github.com/tpope/vim-speeddating)
  * [__vim-eunuch__](https://github.com/tpope/vim-eunuch)
  * [__vim-obsession__](https://github.com/tpope/vim-obsession)

