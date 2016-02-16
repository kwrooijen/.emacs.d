(tempo-define-template
 "elixir-supervisor"
 '("defmodule .Supervisor do"n>
   "use Supervisor"n>
   n>
  "def start_link do"n>
    "Supervisor.start_link(__MODULE__, :ok, name: __MODULE__)"n>
  "end"n>
n>
  "def init(:ok) do"n>
    "children = ["n>
       "# worker(MyWorker, [])"n>
    "]"n>
    "supervise(children, strategy: :one_for_one)"n>
  "end"n>
"end"n>
) nil "Elixir Supervisor Template.")

(tempo-define-template
 "elixir-genserver"
 '("defmodule  do"n>
     "use GenServer"n>
     n>
     "def start_link do"n>
       "GenServer.start_link(__MODULE__, :ok, name: __MODULE__)"n>
     "end"n>
     n>
     "def init(:ok) do"n>
       "{:ok, []}"n>
     "end"n>
   "end"n>
) nil "Elixir GenServer Template.")
