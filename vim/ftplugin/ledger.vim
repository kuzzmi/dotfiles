autocmd BufWritePre *.dat execute "mark i" | execute "%LedgerAlign" | execute "normal 'i"
