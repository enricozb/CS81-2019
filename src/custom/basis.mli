val ty_env : Type.ty Env.env
val val_env : Value.env_value Env.env
val mut_env : bool Env.env

val envs : (Type.ty Env.env * bool Env.env * Value.env_value Env.env )

val basis : string

