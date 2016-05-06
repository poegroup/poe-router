defprotocol PoeRouter.Config.Nginx do
  def to_string(value, opts)

  Kernel.def render(config, opts \\ []) do
    for rule <- config do
      PoeRouter.Config.Nginx.to_string(rule, opts)
    end
  end
end

defimpl PoeRouter.Config.Nginx, for: Atom do
  def to_string(nil, _), do: ""
  def to_string(atom, _) do
    Kernel.to_string(atom)
  end
end
