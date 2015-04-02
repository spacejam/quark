{application, quark,
 [{description, "distributed liveness telemetry"},
  {vsn, "0.0.2"},
  {modules, [
             quark_app,
             gossip_sup,
             election_sup,
             consensus_sup,
             gossip,
             election,
             consensus
            ]},
  {registered, [gossip_sup]},
  {applications, [kernel, stdlib]},
  {mod, {quark_app, []}}
 ]}.
