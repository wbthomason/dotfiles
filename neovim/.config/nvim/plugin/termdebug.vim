let s:termdebug_config = {
      \'delete': ['Termdebug'],
      \'package': 'termdebug',
      \'config': []
      \}

command! -nargs=* Termdebug call util#load_and_run(':Termdebug ' . <q-args>, s:termdebug_config)
