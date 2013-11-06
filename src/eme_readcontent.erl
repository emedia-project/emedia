-module(eme_readcontent).

-export([directory_content/0]).

directory_content() ->
  {ok, Root} = file:get_cwd(),
  filelib:fold_files(Root, ".*", true, fun (FileOrDirPath, Acc) -> [FileOrDirPath|Acc] end, []).
