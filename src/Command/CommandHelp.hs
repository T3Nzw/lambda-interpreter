module CommandHelp where

-- i have no idea what to call the program, so i'll just leave placeholders for now :D hopefully i dont forget to fill them in

generalHelp :: String
generalHelp =
  "Use the h, j, k, l, and q keys to navigate the help pager.\n\
  \Each command in <insert program name here> is prefixed with a colon \":\".\n\
  \Pass the \"-help\" flag to any command to display additional information about the command.\n\
  \Additionally, you can bind lambda terms in the global environment. Terms are evaluated lazily,\n\
  \meaning a term is initially stored as a string and will not be evaluated unless a command specifically requires that.\n\
  \\n\ESC[1mCOMMANDS\ESC[22m\n\
  \<identifier> := <lambda_term>\n\
  \:echo {-help} <string>\n\
  \:lookup {-help}\n\
  \:env {-help}\n\
  \:browse {-help}\n\
  \:eval {-help} <identifier | lambda_term>\n\
  \:sub {-help | -vis} <old_term>; <new_term>; <lambda_term>\n\
  \:alpha {-help | -vis} <identifier | lambda_term>\n\
  \:alphaeq {-help} <identifier> | lambda_term>; <identifier | lambda_term>\n\
  \:beta {-help | -vis} <identifier | lambda_term>\n\
  \:beta1 {-help | -vis} <identifier | lambda_term>\n\
  \:betas {-help | -vis} <identifier | lambda_term>\n\
  \:beta1s {-help | -vis} <identifier | lambda_term>\n\
  \:eta {-help | -vis} <identifier | lambda_term>\n\
  \:liftmin {-help | -vis} <identifier | lambda_term>\n\
  \:liftmax {-help | -vis} <identifier | lambda_term>\n\
  \:nameless {-help | -vis} <identifier | lambda_term>\n\
  \:lnameless {-help | -vis} <identifier | lambda_term>\n\
  \:t {-help | -vis} <identifier | lambda_term>\n\b"

echoHelp :: String
echoHelp = "\ESC[93m:echo {-help} <string>\nOutput <string> sequence of characters to the console. Optionally pass the \"-help\" flag to see a brief description of the command."

envHelp :: String
envHelp =
  "\ESC[93m:env {-help}\nOutput all bindings in the global environment. Each binding consists of a key (identifier)\n\
  \and a value (either a string or a lambda term, depending on whether the term has been evaluated or not).\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command."

browseHelp :: String
browseHelp =
  "\ESC[93m:browse {-help}\nOpen up a menu with all the available commands. Press enter to select a command.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command."

evalHelp :: String
evalHelp =
  "\ESC[93m:eval {-help} <idenfitier | lambda_term>\nEvaluate a lambda term. If an identifier is passed, look up the corresponding value \
  \in the global environment.\nIf the idenfitier is present, try to evaluate the value, if it is not already evaluated, and overwrite the existing one.\n\
  \If the evaluation process fails, an error is displayed and the binding is removed from the global environment.\n\
  \If the argument is not a valid identifier, try to evaluate it as a raw lambda term."

substituteHelp :: String
substituteHelp =
  "\ESC[93m:sub {-help | -vis} <old_term>; <new_term>; <lambda_term>\n\
  \Replace all occurences of <old_term> in <lambda_term> with <new_term> using a capture-avoiding substitution.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the substitution."

alphaHelp :: String
alphaHelp =
  "\ESC[93m:alpha {-help | -vis} <identifier | lambda_term>\n\
  \Alpha-convert a lambda term. If an identifier is passed, it is looked up in the global environment.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the alpha-conversion."

alphaEqHelp :: String
alphaEqHelp =
  "\ESC[93m:alphaeq {-help} <identifier | lambda_term>; <identifier | lambda_term>\n\
  \Check if two lambda terms are alpha equivalent using normalisation and de Bruijn indices.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command."

-- TODO: allow for changing the upper limit on the single-step beta-reductions executed
betaHelp :: String
betaHelp =
  "\ESC[93m:beta {-help | -vis} <identifier | lambda_term>\n\
  \Attempt to fully beta-reduce a lambda term using a normal reduction strategy.\n\
  \If the number of single-step beta-reduction processes exceeds the global limit (see :brlimit),\n\
  \the process terminates and outputs an appropriate message as the term could be potentially irreducable.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display the reduction process step by step."

beta1Help :: String
beta1Help =
  "\ESC[93m:beta1 {-help | -vis} <identifier | lambda_term>\n\
  \Perform single-step beta-reduction using a normal reduction strategy.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the single-step beta-reduction."

betaSHelp :: String
betaSHelp =
  "\ESC[93m:betas {-help | -vis} <identifier | lambda_term>\n\
  \Attempt to fully beta-reduce a lambda term using a strict reduction strategy.\n\
  \If the number of single-step beta-reduction processes exceeds the global limit (see :brlimit),\n\
  \the process terminates and outputs an appropriate message as the term could potentially be irreducable.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display the reduction process step by step."

beta1SHelp :: String
beta1SHelp =
  "\ESC[93m:beta1s {-help | -vis} <identifier | lambda_term>\n\
  \Perform single-step beta-reduction using a strict evaluation strategy.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the single-step beta-reduction."

etaHelp :: String
etaHelp =
  "\ESC[93m:eta {-help | -vis} <identifier | lambda_term>\n\
  \Fully eta-reduce a lambda term.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display the reduction process step by step."

eta1Help :: String
eta1Help =
  "\ESC[93m:eta1 {-help | -vis} <identifier | lambda_term>\n\
  \Perform single-step eta-reduction on a lambda term.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the single-step eta-reduction."

liftmaxHelp :: String
liftmaxHelp =
  "\ESC[93m:liftmax {-help | -vis} <identifier | lambda_term>\n\
  \Perform maximal lambda lifting on a lambda term.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the command."

liftminHelp :: String
liftminHelp =
  "\ESC[93m:liftmin {-help | -vis} <identifier | lambda_term>\n\
  \Perform minimal lambda lifting on a lambda term.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the command."

namelessHelp :: String
namelessHelp =
  "\ESC[93m:nameless {-help | -vis} <identifier | lambda_term>\n\
  \Convert a named lambda term to its nameless representation using de Bruijn indices.\n\
  \Uses a slightly more clever approach that dynamically constructs a context of all free variables.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the command."

locallyNamelessHelp :: String
locallyNamelessHelp =
  "\ESC[93m:lnameless {-help | -vis} <identifier | lambda_term>\n\
  \Convert a lambda term to its locally nameless representation based on de Bruijn indices\n\
  \where all free variables keep their names and only bound variables are indexed.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display a colour-coded diff visualisation of the command."

applicativeHelp :: String
applicativeHelp =
  "\ESC[93m:app {-help } <identifier | lambda_term>\n\
  \Convert a lambda term to its applicative term representation (SK-calculus).\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command."

inferHelp :: String
inferHelp =
  "\ESC[93m:t {-help | -vis} <identifier | lambda_term>\n\
  \Infer the type of a lambda term in the simply-typed lambda calculus, if it exists.\n\
  \Optionally pass the \"-help\" flag to see a brief description of the command,\n\
  \or the \"-vis\" flag to display the type inference process step by step,\n\
  \formatted with respect to the generated AST. (AST?)"
