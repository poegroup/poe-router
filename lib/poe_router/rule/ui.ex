defmodule PoeRouter.Rule.UI do
  defstruct backend: nil,
            auth_url: nil,
            api_url: nil,
            path: "/"
end

defimpl PoeRouter.Config.Nginx, for: PoeRouter.Rule.UI do
  def to_string(_, _) do
    ""
  end
end
