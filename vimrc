
let uname = substitute(system("uname"),"\n","","g")

let mapleader = "," " change from \ to ,

" Quickly edit/reload the vimrc file
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>

"
" Some distros set filetype indent too early. Turn it off before
" calling pathogen.
"
" Example module management with git:
"
" $ git submodule add http://github.com/scrooloose/nerdtree.git bundle/nerdtree
" $ git submodule init
" $ git commit -m 'add nerd tree plugin as submodule'
"
" To pull into another machine:
"
" $ git submodule update --init
"
filetype off
call pathogen#runtime_append_all_bundles()

"
" Color Schemes
"
if has("gui_running")
  set background=light
  colorscheme autumn
  syntax on
else
  if &term =~ "xterm"
    set t_Co=8
    set t_Sf=[3%p1%dm
    set t_Sb=[4%p1%dm
  endif

  if &term =~ "screen*" || &term =~ "xterm-256" || uname == "Darwin"
    set title
    set t_Co=256
    set t_ts=]2;
    set t_fs=
  endif

  if &t_Co > 2
    set background=light
    syntax on
  endif

  if &t_Co >= 256
    colorscheme peaksea
  endif
endif

"
" Basic Stuff
"
set nocompatible " explicitly get out of vi-compatible mode
set noexrc " don't load vimrc found in current directory
set backspace=indent,eol,start " make backspace flexible
set hidden " change buffer without saving
set noerrorbells " don't make noise
set novisualbell " dont' blink
set nobackup " don't make a backup before overwriting a file
set wildmenu " turn on command line completion
set wildmode=list:longest " list all matches until last one standing
set wildignore+=*.class,*.o,*.obj,*.d,*.hh,*.png,*.swf,*.gif,*.jpg,*.fla,*.mp3,*.jar " ignore these files
set laststatus=2 " show status line even with one window left
set list " show tabs and trailing spaces
set listchars=tab:>-,trail:-
set ruler " show the cursor position all the time
set showmatch " jump to matching bracket briefly
set title " set title to the value of titlestring
set modelines& " restore modelines to 5 on OS X
if has("win32") || has("win64")
  set directory=$TMP
else
  set directory=~/tmp//,/var/tmp//,/tmp//
endif

if v:version >= 701

elseif v:version >= 700

elseif v:version >= 600

endif

"
" GUI-related Stuff
"
if has("gui_running")
  set guioptions-=T " include toolbar
  set guioptions-=m " include menubar
  set guioptions-=l " exclude scrollbars
  set guioptions-=r " exclude scrollbars
  set guioptions-=L " exclude scrollbars
  set guioptions-=R " exclude scrollbars
  set mouse=a
  set clipboard=autoselect
else
  set clipboard=exclude:screen.*
endif

"
" Indenting behaviors
"
set autoindent
set smarttab
set smartindent
filetype plugin indent on

set pastetoggle=<leader>pt
set textwidth=78
set statusline=%<%f\ %y%m%r%=%-14.(%l,%c%V%)\ %P " add filetype to status line

set viminfo='20,\"50 " don't store more than 50 lines of registers
set history=50 " keep 50 lines of command line history

set tags=./tags;/ " search tags from directory of the current file upwards until found

"
" Macros
"

" Edit another file in the same directory.
if has("unix") 
  map <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
else
  map <leader>e :e <C-R>=expand("%:p:h") . "\\" <CR>
endif


nn <leader>d :bd<CR>
nn <leader>c <C-W>c

"
" Plugins
"
map <leader>b :BufExplorer<cr>
map <leader>s :SBufExplorer<cr>
map <leader>v :VSBufExplorer<cr>

" Fix a color bug with vim and screen. If vim has set a background color the
" console still use the color after vim quits.
if !has("gui_running")
  augroup fix_gnu_screen
    autocmd!
    autocmd VimLeave * :set term=screen
  augroup END
endif

if has("cscope")
  " Use quick fix window
  set cscopequickfix=s-,c-,d-,i-,t-,e-
endif

autocmd FileType php set shiftwidth=2 expandtab tabstop=2
autocmd FileType python set shiftwidth=2 expandtab tabstop=2
autocmd FileType java set shiftwidth=4 expandtab tabstop=4
autocmd FileType perl set shiftwidth=2 expandtab tabstop=2
autocmd FileType javascript set shiftwidth=2 expandtab tabstop=2
autocmd FileType html set shiftwidth=2 expandtab tabstop=2
autocmd FileType haskell set shiftwidth=4 expandtab tabstop=4
autocmd FileType c set nolist

"
" Source in local vimrc if exists.
"
let localvimrc=expand("~/.vim/vimrc.local")
if filereadable(localvimrc)
  exec "source " . localvimrc
endif

set nobackup
set nowritebackup

let g:CommandTMaxFiles=50000

" vim:tw=78:ts=2:et:sw=2
