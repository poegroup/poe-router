defmodule PoeRouter.Domain do
  defstruct domain: nil

  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__)
      @before_compile unquote(__MODULE__)
      Module.register_attribute(__MODULE__, :__config__, accumulate: true)
    end
  end

  defmacro marketing(opts \\ []) do
    quote do
      %PoeRouter.Rule.Marketing{backend: config(:"#{unquote(opts[:name] || :marketing)}_url")}
    end |> push(__CALLER__)
  end

  defmacro favicon do
    [
      "/favicon.ico",
      "/apple-touch-icon-precomposed.png",
      "/apple-touch-icon-72x72-precomposed.png",
      "/apple-touch-icon-76x76-precomposed.png",
      "/apple-touch-icon-114x114-precomposed.png",
      "/apple-touch-icon-120x120-precomposed.png",
      "/apple-touch-icon-144x144-precomposed.png",
      "/apple-touch-icon-152x152-precomposed.png"
    ] |> Enum.map(fn(path) ->
      quote do
        file(unquote(path), "#{config(:static_directory)}#{config(:favicon_directory, optional: true)}#{unquote(path)}")
      end
    end)
  end

  defmacro robots(opts \\ []) do
    default = opts[:default] || "/robots.txt"
    quote do
      file("/robots.txt", "#{config(:static_directory)}#{config(:robots) || unquote(default)}")
    end
  end

  defmacro ui(name, path, opts \\ []) do
    quote do
      %PoeRouter.Rule.UI{backend: config(:"ui_#{unquote(name)}_url"),
                    auth_url: config(unquote(opts[:auth] || :auth_url), optional: true),
                    api_url: config(unquote(opts[:api] || :api_url), optional: true),
                    path: unquote(path)}
    end |> push(__CALLER__)
  end

  defmacro redirect(from, to, opts \\ []) do
    quote do
      %PoeRouter.Rule.Redirect{from: unquote(from),
                          to: unquote(to),
                          type: unquote(opts[:type])}
    end |> push(__CALLER__)
  end

  defmacro file(path, file, opts \\ []) do
    quote do
      %PoeRouter.Rule.File{path: unquote(path),
                      file: unquote(file),
                      max_age: unquote(opts[:max_age]),
                      content_type: unquote(opts[:content_type])}
    end |> push(__CALLER__)
  end

  defmacro config(name, opts \\ []) do
    name = eval_config_name(name, __CALLER__)
    quote do
      case Map.get(var!(__domain_config__), unquote(name)) do
        nil ->
          unquote(if opts[:optional] do
            nil
          else
            quote do
              raise PoeRouter.MissingConfig, name: unquote(name)
              nil
            end
          end)
        value ->
          value
      end
    end
  end

  defp push(ast, env) do
    module = env.module
    name = :"_#{:erlang.phash2(ast)}"

    Module.register_attribute(module, :__funs__, accumulate: true)
    Module.put_attribute(module, :__funs__, name)

    quote do
      defp unquote(name)(var!(__domain_config__), var!(__global_config__)) do
        unquote(ast)
      end
    end
  end

  defp eval_config_name(name, env, transform \\ &(&1)) do
    name = PoeRouter.Utils.eval(name, env) |> transform.()
    module = env.module
    Module.register_attribute(module, :__config__, accumulate: true)
    Module.put_attribute(module, :__config__, name)
    name
  end

  defmacro __before_compile__(_) do
    quote unquote: false do
      config = __MODULE__
      |> Module.get_attribute(:__config__)
      |> Enum.uniq
      |> Enum.map(&{&1, nil})

      require Logger

      def get_config(name) do
        unquote({:%{}, [], config})
      end

      funs = Module.get_attribute(__MODULE__, :__funs__) |> Enum.reverse()
      def expand(name, config) do
        unquote(for fun <- funs do
          quote do
            try do
              unquote(fun)(config[name] || %{}, config)
            rescue
              e in PoeRouter.MissingConfig ->
                Exception.format(:error, e, System.stacktrace)
                |> Logger.warn()
                nil
            end
          end
        end)
      end
    end
  end
end
