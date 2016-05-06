defmodule PoeRouter do
  defmacro __using__(_) do
    quote do
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      Module.register_attribute __MODULE__, :__domains__, accumulate: true
    end
  end

  defmacro domain(name, module) do
    quote do
      Module.put_attribute __MODULE__, :__domains__, {unquote(name), unquote(module)}
    end
  end

  defmacro __before_compile__(_) do
    quote unquote: false do
      domains = Module.get_attribute(__MODULE__, :__domains__) |> Enum.reverse()
      def config do
        for {name, module} <- unquote(domains) do
          {name, module.get_config(name)}
        end |> :maps.from_list()
      end

      def render(config) do
        unquote(domains)
        |> Enum.flat_map(fn({name, module}) ->
          module.expand(name, config)
        end)
        ## TODO wrap the whole thing
      end
    end
  end
end
