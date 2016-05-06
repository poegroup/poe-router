defmodule PoeRouter.MissingConfig do
  defexception [:name, :domain]

  def message(%{name: name, domain: domain}) do
    "Missing configuration value #{inspect(name)}#{if domain do
      " for domain #{inspect(domain)}"
    end}"
  end
end
