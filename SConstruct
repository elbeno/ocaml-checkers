env = Environment(
  OCAML_PACKS='lablgtk2',
  OCAML_EXTRA_OBJS='gtkInit',
  OCAML_CODE='bytecode',
  OCAML_DEBUG=1,
  OCAML_PROFILE=0
)
env.Tool('ocaml', '.')
env.OcamlProgram('checkers', 'checkers.ml')
