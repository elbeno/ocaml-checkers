env = Environment(
  OCAML_PACKS=['lablgtk2'], #'unix', 'camomile', 'str', 'batteries'],
  OCAML_EXTRA_OBJS='gtkInit',
  OCAML_CODE='native',
  OCAML_DEBUG=1,
  OCAML_PROFILE=0
)
env.Tool('ocaml', '.')
env.OcamlProgram('checkers', Glob('*.ml'))
