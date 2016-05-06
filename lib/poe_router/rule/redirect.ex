defmodule PoeRouter.Rule.Redirect do
  defstruct from: "/",
            to: "/",
            type: :temporary
end

defimpl PoeRouter.Config.Nginx, for: PoeRouter.Rule.Redirect do
  alias PoeRouter.Utils

  def to_string(%{from: nil}, _), do: ""
  def to_string(%{to: nil}, _), do: ""
  def to_string(%{from: from, to: to, type: type}, _) do
    """
    location = #{from} {
      rewrite             #{from} #{to} #{type};
    }
    location #{Utils.join(from, "")} {
      rewrite             #{Utils.join(from, "(.*)")} #{Utils.join(to, "$1")} #{type};
    }
    """
  end
end
