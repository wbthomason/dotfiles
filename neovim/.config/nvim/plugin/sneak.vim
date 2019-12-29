" 2-character Sneak (default)
nmap z <Plug>Sneak_s
nmap Z <Plug>Sneak_S

" visual-mode
xmap z <Plug>Sneak_s
xmap Z <Plug>Sneak_S

" operator-pending-mode
omap z <Plug>Sneak_s
omap Z <Plug>Sneak_S

" repeat motion
map ; <Plug>Sneak_;
map \ <Plug>Sneak_,

" 1-character enhanced 'f'
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F

" visual-mode
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F

" operator-pending-mode
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F

" 1-character enhanced 't'
nmap t <Plug>Sneak_t
nmap T <Plug>Sneak_T

" visual-mode
xmap t <Plug>Sneak_t
xmap T <Plug>Sneak_T

" operator-pending-mode
omap t <Plug>Sneak_t
omap T <Plug>Sneak_T

let g:sneak#s_next = 1
let g:sneak#label = 1
let g:sneak#use_ic_scs = 1
