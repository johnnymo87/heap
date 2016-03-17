nnoremap ,oo :!cabal build<CR>
nnoremap ,0k :wa<CR>:!cabal exec -- runhaskell -isrc -itest test/Spec.hs<CR>
