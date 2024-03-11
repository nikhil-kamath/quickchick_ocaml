(* Global setup functions *)

let setup =
  ref
    Required.
      {
        extractions = [];
        libraries = [];
        ml_file = "";
        temp_dir = "";
        target = "";
      }

let setup_add_library lib =
  setup :=
    {
      extractions = !setup.extractions;
      libraries = lib :: !setup.libraries;
      ml_file = !setup.ml_file;
      temp_dir = !setup.temp_dir;
      target = !setup.target;
    }

let setup_add_file file =
  setup :=
    {
      extractions = Required.Extractions.File file :: !setup.extractions;
      libraries = !setup.libraries;
      ml_file = !setup.ml_file;
      temp_dir = !setup.temp_dir;
      target = !setup.target;
    }

let setup_add_type package type_name constructors =
  setup :=
    {
      extractions =
        Required.Extractions.(Ty { package; type_name; constructors })
        :: !setup.extractions;
      libraries = !setup.libraries;
      ml_file = !setup.ml_file;
      temp_dir = !setup.temp_dir;
      target = !setup.target;
    }

let setup_add_function package function_name =
  setup :=
    {
      extractions =
        Required.Extractions.(Func { package; function_name })
        :: !setup.extractions;
      libraries = !setup.libraries;
      ml_file = !setup.ml_file;
      temp_dir = !setup.temp_dir;
      target = !setup.target;
    }
