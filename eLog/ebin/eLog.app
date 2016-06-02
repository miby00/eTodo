{application, eLog,
 [
  {description, "Simple logging application"},
  {vsn, "1.0.0"},
  {registered, [eLog, eLogWriter]},
  {modules, [
      eLog_app,
      eLog_sup,
      eLog,
      eLogWriter
   ]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { eLog_app, []}},
  {env, []}
 ]}.
