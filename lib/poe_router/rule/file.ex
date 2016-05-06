defmodule PoeRouter.Rule.File do
  defstruct path: nil,
            file: nil,
            max_age: 31_536_000,
            content_type: nil
end

defimpl PoeRouter.Config.Nginx, for: PoeRouter.Rule.File do
  def to_string(%{path: nil}, _), do: ""
  def to_string(%{file: nil}, _), do: ""
  def to_string(%{path: path, file: file, max_age: max_age, content_type: content_type}, _) do
    """
    location = #{path} {
      alias             #{file};
      add_header        cache-control "max-age=#{max_age || 31_536_000}, public";#{content_type(content_type)}
    }
    """
  end

  defp content_type(content_type) do
    if content_type do
      "\n  add_header        content-type #{content_type};"
    end
  end
end
