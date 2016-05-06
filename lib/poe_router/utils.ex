defmodule PoeRouter.Utils do
  def eval(quoted, env) do
    {out, []} = quoted
    |> Macro.expand(env)
    |> Code.eval_quoted([], env)
    out
  end

  def join(a, b, char \\ "/")
  def join(nil, b, _) do
    to_string(b)
  end
  def join(a, nil, _) do
    to_string(a)
  end
  def join(a, b, char) do
    if String.ends_with?(a, char) do
      "#{a}#{b}"
    else
      "#{a}#{char}#{b}"
    end
  end
end
