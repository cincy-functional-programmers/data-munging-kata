defmodule FootballReport do
  @score_dataregex ~r/^\s*\d+\. (\w+)\s+(\d+\s+){4}(\d+)  -  (\d+)/

  def main(data_file) do
    result = DataExtract.find_smallest_diff(data_file, @score_dataregex, &extract_score_data/1)
    IO.puts(result)
  end

  defp extract_score_data([_, team, _, points_for, points_against]) do
    {team, points_for, points_against}
  end
end
