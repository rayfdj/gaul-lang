if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal commentstring=//\ %s
setlocal comments=://,s1:/*,mb:*,ex:*/
setlocal tabstop=2 shiftwidth=2 expandtab
setlocal suffixesadd=.gaul
