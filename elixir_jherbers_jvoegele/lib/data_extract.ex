defmodule DataExtract do
  def find_smallest_diff(data_file, regex, extract_fun) do
    File.stream!(data_file)
    |> extract_matching_lines(regex, extract_fun)
    |> extract_min
  end

  def extract_matching_lines(stream, regex, map_fn) do
    stream
    |> Stream.map(fn(line) -> Regex.run(regex, line) end)
    |> Stream.filter(&(&1 != nil))
    |> Stream.map(map_fn)
    |> Enum.to_list
  end

  @spec extract_min([{String.t, String.t, String.t}]) :: String.t
  def extract_min(data) do
    {result, _, _} = Enum.min_by(data, fn({_, x, y}) -> abs(to_int(x) - to_int(y)) end)
    result
  end

  defp to_int(string) do
    {num, _} = Integer.parse(string)
    num
  end
end
