{application, quark,
 [{description, "distributed liveness telemetry"},
  {vsn, "0.0.1"},
  {modules, [
             q_app,
             q_sup,
             q_cache,
             q_detect,
             q_store
            ]},
  {registered, [q_sup]},
  {applications, [kernel, stdlib]},
  {mod, {q_app, []}}
 ]}.
