Erlang 编码标准指引
====================================

Suggested reading material: http://www.erlang.se/doc/programming_rules.shtml

***

Table of Contents:
* [Contact Us](#contact-us)
* [翻译贡献者](#翻译贡献者)
* [约定 &amp; 规则](#约定--规则)
   * [源码布局](#源码布局)
      * [用空格代替制表符(tab)](#用空格代替制表符tab)
      * [使用你的空格键](#使用你的空格键)
      * [行尾不要留空格](#行尾不要留空格)
      * [每行100列](#每行100列)
      * [保持现有风格](#保持现有风格)
      * [避免多层嵌套](#避免多层嵌套)
      * [更多, 小函数比 case 表达式好用](#更多-小函数比-case-表达式好用)
      * [函数按逻辑功能分组](#函数按逻辑功能分组)
      * [集中你的 types](#集中你的-types)
      * [不要上帝模块](#不要上帝模块)
      * [简洁的单元测试](#简洁的单元测试)
      * [Honor DRY](#honor-dry)
      * [避免动态调用](#避免动态调用)
      * [使用文件夹将功能性相关的模块分组](#使用文件夹将功能性相关的模块分组)
      * [不要写面条式代码](#不要写面条式代码)
   * [语法](#语法)
      * [避免使用 if 表达式](#避免使用-if-表达式)
      * [避免嵌套 try...catches](#避免嵌套-trycatches)
   * [命名](#命名)
      * [在命名概念时保持一致](#在命名概念时保持一致)
      * [Explicit state should be explicitly named](#explicit-state-should-be-explicitly-named)
      * [Don't use _Ignored variables](#dont-use-_ignored-variables)
      * [避免用布尔类型作为函数参数](#避免用布尔类型作为函数参数)
      * [Stick to one convention for naming modules](#stick-to-one-convention-for-naming-modules)
      * [原子(atoms)请用小写](#原子atoms请用小写)
      * [函数名](#函数名)
      * [变量名](#变量名)
   * [字符串](#字符串)
      * [IOLists over string concatenation](#iolists-over-string-concatenation)
   * [宏](#宏)
      * [宏的应用场景](#宏的应用场景)
      * [宏名要大写](#宏名要大写)
      * [模块或函数名不能用宏命名](#模块或函数名不能用宏命名)
   * [记录(Records)](#记录records)
      * [记录(record) 命名](#记录record-命名)
      * [记录(record)先行](#记录record先行)
      * [不要共享记录(record)](#不要共享记录record)
      * [在 specs 里避免出现记录(record)](#在-specs-里避免出现记录record)
      * [记录(record)的类型(Types)](#记录record的类型types)
   * [其它](#其它)
      * [Write function specs](#write-function-specs)
      * [Use -callback attributes over behaviour_info/1](#use--callback-attributes-over-behaviour_info1)
      * [Use atoms or tagged tuples for messages](#use-atoms-or-tagged-tuples-for-messages)
      * [No nested header inclusion](#no-nested-header-inclusion)
      * [在头文件里不要有类型定义](#在头文件里不要有类型定义)
      * [不要用 import](#不要用-import)
      * [不要用 export_all](#不要用-export_all)
      * [Encapsulate OTP server APIs](#encapsulate-otp-server-apis)
      * [No debug calls](#no-debug-calls)
      * [Don't Use Case Catch](#dont-use-case-catch)
   * [工具](#工具)
      * [锁定你的依赖](#锁定你的依赖)
      * [Loud errors](#loud-errors)
      * [Properly use logging levels](#properly-use-logging-levels)
      * [Prefer the https protocol over others when specifying dependency URLs](#prefer-the-https-protocol-over-others-when-specifying-dependency-urls)
* [好的建议和方法](#好的建议和方法)
  * [优先使用高级函数而不是手写的递归方法](#优先使用高级函数而不是手写的递归方法)
  * [驼峰式命名,下划线命名](#驼峰式命名-下划线命名)
  * [更短 (但仍保持有意义的) 的变量名称](#更短-但仍保持有意义的-的变量名称)
  * [注释等级](#注释等级)
  * [保持函数精简](#保持函数精简)
  * [使用行为模式.](#使用行为模式)
  * [在发起的一方做防御编程](#在发起的一方做防御编程)
  * [避免不必要调用length/1](#避免不必要调用length1)
  * [Move stuff to independent applications](#move-stuff-to-independent-applications)
  * [Use the facade pattern on libraries](#use-the-facade-pattern-on-libraries)
  * [Types in exported functions](#types-in-exported-functions)
  * [Separate responsibilities in sumo_db](#separate-responsibilities-in-sumo_db)

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please [open an issue](https://github.com/inaka/erlang_guidelines/issues/new) in this repo (or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)

## 翻译贡献者

>  本编码规范基于inaka公司的[erlang_guidelines](https://github.com/inaka/erlang_guidelines)

[@feng19](https://github.com/feng19) \ [@JoeLeewell](https://github.com/Baymask) \ [@huangjialegaoan](https://github.com/huangjialegaoan) \ [@hjh](https://github.com/hjh2010) 排名不分先后

## 约定 & 规则

"Things that may be used as reason to reject a Pull Request."

### 源码布局

***
#### 用空格代替制表符(tab)
> 用空格代替制表符(tab),使用两个空格符作为缩进.

*Examples*: [indent](src/indent.erl)

```erlang
%% @doc 不一致
bad() ->
  try
    ThisBlock = is:indented(with, two, spaces),
    that:is_good(ThisBlock) %% 这一部分的代码缩进用两个空格,没啥毛病
  catch
      _:_ ->
          this_block:is_indented(with, four, spaces) %% 但是这一部分的却用了4个空格,看起来不统一,很糟糕
  end.

%% @doc 一致,但是使用4个空格
better() ->
    receive
        {this, block} -> is:indented(with, four, spaces);
        _That -> is:not_good() %% 这一部分的代码缩进用四个空格,不太好
    after 100 ->
        but:at_least(it, is, consistent) %% 但起码全部是使用一致的风格
    end.

%% @doc 不错
good() ->
  case indentation:block() of
    {2, spaces} -> me:gusta();
    {_, _} -> not_sure:if_gusta()
  end.
```

*原因*: 这并不意味着允许代码中存在多层嵌套的结构.如果代码足够干净,2个空格就足够了,代码看起来更加简洁,同时在同一行中也能容纳更多的字符.

***
#### 使用你的空格键
> 使用空格来分割开运算符和逗号.

*Examples*: [spaces](src/spaces.erl)

```erlang
% @doc 没有空格
bad(_My,_Space,_Bar)->[is,'not',working].

% @doc 带空格!!
good(_Hey, _Now, _It) -> ["works " ++ "again, " | [hooray]].
```

*原因*: 同上,主要是为了代码易于读写,等等.

***
#### 行尾不要留空格
> 检查你的没一行代码的最后,不要有空格.

*Examples*: [trailing_whitespace](src/trailing_whitespace.erl)

```erlang
bad() -> "这行尾部有空格".       

good() -> "这行没有".
```

*原因*: 这是提交噪音. 可以看看[长篇论据](https://programmers.stackexchange.com/questions/121555/why-is-trailing-whitespace-a-big-deal).

#### 每行100列
> 每行最多100个字符.

*Examples*: [col_width](src/col_width.erl)

```erlang
%$ @doc 太宽
bad([#rec{field1 = FF1, field2 = FF2, field3 = FF3}, #rec{field1 = BF1, field2 = BF2, field3 = BF3} | Rest], Arg2) ->
  other_module:bad(FF1, FF2, FF3, BF1, BF2, BF3, bad(Rest, Arg2)).

%% @doc 不错 (< 100 字符)
good([Foo, Bar | Rest], Arg2) ->
  #rec{field1 = FF1, field2 = FF2, field3 = FF3} = Foo,
  #rec{field1 = BF1, field2 = BF2, field3 = BF3} = Bar,
  other_module:good(FF1, FF2, FF3, BF1, BF2, BF3, good(Rest, Arg2)).
```

*原因*: 太长的行在处理的时候是相当痛苦的: 要么在编辑的时候不停水平滚动, 要么就是忍受自动断行造成布局错乱.
100个字符的限制不仅仅让每一行保持简短, 另外也能让你可以毫无压力地在标准的手提电脑屏幕上并排同时打开两个文件, 或者三个 1080p 显示器上.

***
#### 保持现有风格
> 当你维护别人的模块时, 请坚持按前人的编码风格样式维护. 如果项目有整体的风格样式, 那么在编写新的模块是也要坚持按项目的整体风格进行.

*Examples*: [existing_style](src/existing_style.erl)

```erlang
bad() ->
  % 之前的代码
  List = [ {elem1, 1}
         , {elem2, 2}
  % 新代码 (不按之前的格式来编码)
         , {elem3, 3}, {elem4, 4},
           {elem5, 5}
         ],
  other_module:call(List).

good() ->
  % 之前的代码
  List = [ {elem1, 1}
         , {elem2, 2}
  % 新代码 (按之前的格式来编码)
         , {elem3, 3}
         , {elem4, 4}
         , {elem5, 5}
         ],
  other_module:call(List).
```

*原因*: 在维护别人的代码的时候,如果你不喜欢他的编码规范,这仅仅是你个人不喜欢而已,但是如果你不按他之前写的编码样式继续编写,那这个模块就有两种编码样式了,这样你本人看起来这些代码很丑陋,别人看你的代码也觉得很丑陋,这样会让代码更加不容易维护.

***

#### 避免多层嵌套
> 尽量不要出现超过三个层级嵌套的代码样式

*Examples*: [nesting](src/nesting.erl)

```erlang
bad() ->
  case this:function() of
    has ->
      try too:much() of
        nested ->
          receive
            structures ->
              it:should_be(refactored);
            into ->
              several:other(functions)
          end
      catch
        _:_ ->
          dont:you("think?")
      end;
    _ ->
      i:do()
  end.

good() ->
  case this:function() of
    calls ->
      other:functions();
    that ->
      try do:the(internal, parts) of
        what ->
          was:done(in)
      catch
        _:the ->
          previous:example()
      end
  end.

%% 译者注: 上面部分代码的意思:通过将嵌套部分的代码封装成一些新的函数,可以减少嵌套的结构.
```

*原因*: 嵌套级别表示函数中的逻辑比较复杂,过多地将需要执行和完成的决策放在单个函数中. 这不仅阻碍了可读性,而且阻碍了可维护性(在进行更改时)和调试以及编写单元测试的进行.
与之相关: [More, smaller functions over case expressions](#more-smaller-functions-over-case-expressions).

***
#### 更多, 小函数比 case 表达式好用
> 使用模式匹配的函数子句代替 case 表达式. 特别是当 case 在:
> - 函数的最上层(下面代码第一个bad函数)
> - 巨大的

*Examples*: [smaller_functions](src/smaller_functions.erl)

```erlang
%% @doc 这个函数仅仅使用的是 case 表达式
bad(Arg) ->
  case Arg of
    this_one -> should:be(a, function, clause); %% 这一句应该用一个函数子句代替
    and_this_one -> should:be(another, function, clause) %% 这一句应该用另一个函数子句代替
  end.

%% @doc 使用模式匹配
good(this_one) -> is:a(function, clause); %% 这是一个函数子句
good(and_this_one) -> is:another(function, clause). %% 这是另一个函数子句


%% @doc case 表达式在函数内部
bad() ->
  InitialArg = some:initial_arg(),
  InternalResult =
    case InitialArg of
      this_one -> should:be(a, function, clause);
      and_this_one -> should:be(another, function, clause)
    end,
  some:modification(InternalResult).

%% @doc 使用多个函数字句代替内部 case 表达式
good() ->
  InitialArg = some:initial_arg(),
  InternalResult = good(InitialArg),
  some:modification(InternalResult).
```

*原因:* 一般而已,函数体中的一个case代表某种决定,同时函数应尽可能的简单. 如果决策结果的每个分支作为一个函数子句而不是一个case子句来实现,同时函数子句的函数名也可以让代码容易读懂. 换言之, 这个 case 在此扮演的是 '匿名函数', 除非它们在高阶函数的上下文中被使用,而只是模糊的含义.

***
#### 函数按逻辑功能分组
> 始终保持区分导出函数和未导出的函数, 并将导出的放在前面, 除非还有其他方法更加有助于可读性和代码发现的.

*Examples*: [grouping_functions](src/grouping_functions)

`bad.erl`:

```erlang
%%% @doc 私有和公用函数随意摆放
-module(bad).

-export([public1/0, public2/0]).

public1() -> private3(atom1).

private1() -> atom2.

public2() -> private2(private1()).

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

`better.erl`:

```erlang
%%% @doc 按函数相关程度区分组
-module(better).

-export([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> public1(X);
    _ -> throw(cant_do)
  end.
%% @doc 这是一个仅仅与上面函数相关的私有函数
public1(X) -> private3(X).

public2() -> private2(private1()).

private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

`good.erl`:

```erlang
-module(good).

-export([public1/0, public2/0]).

public1() ->
  case application:get_env(atom_for_public_1) of
    {ok, X} -> private3(X);
    _ -> throw(cant_do)
  end.

public2() -> private2(private1()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

private1() -> atom2.

private2(Atom) -> private3(Atom).

private3(Atom) -> Atom.
```

*原因*: 好的代码结构易于读/理解/修改.

***
#### 集中你的 types
> 将 types 都放在文件开头的地方

*Examples*: [type_placement](src/type_placement.erl)

```erlang
-type good_type() :: 1..3.

-spec good() -> good_type().
good() -> 2.


-type bad_type() :: 1..3.
-spec bad() -> bad_type().
bad() -> 2.
```

*原因*: Types 定义的数据结构极有可能被用于多个函数,所以他们的定义不能只与其中一个有关. 另外将他们在代码中放在一起并像文档一样展示他们就像 edoc 也是将 types 放在每个文档的开头一样.

***
#### 不要上帝模块
> 不要让你的系统使用上帝模块 (模块中包含了很多函数 和/或 函数与函数之间处理的事情并不相关)

*Examples*: [god](src/god.erl)

```erlang
%%% @doc all of your db operations belong to us!
-module(god).

-export([create_user/1, create_user/2, create_user/3]).
-export([update_user/2, update_user/3]).
-export([delete_user/1]).
-export([create_post/1, create_post/2, create_post/3]).
-export([update_post/2, update_post/3]).
-export([delete_post/1]).
-export([create_comment/2, create_comment/3]).
-export([update_comment/3, update_comment/4]).
-export([delete_comment/2]).

create_user(Name) -> create_user(Name, undefined).

create_user(Name, Email) -> create_user(Name, Email, undefined).

create_user(Name, Email, Phone) ->
  some_db:insert(users, [{name, Name}, {email, Email}, {phone, Phone}]).

update_user(Name, Changes) ->
  some_db:update(users, [{name, Name}], Changes).

update_user(Name, Key, Value) ->
  update_user(Name, [{Key, Value}]).

delete_user(Name) ->
  some_db:delete(users, [{name, Name}]).

create_post(Text) -> create_post(Text, undefined).

create_post(Text, Title) -> create_post(Text, Title, undefined).

create_post(Text, Title, Image) ->
  some_db:insert(posts, [{text, Text}, {title, Title}, {image, Image}]).

update_post(Text, Changes) ->
  some_db:update(posts, [{text, Text}], Changes).

update_post(Text, Key, Value) ->
  update_post(Text, [{Key, Value}]).

delete_post(Text) ->
  some_db:delete(posts, [{text, Text}]).

create_comment(PostId, Text) -> create_comment(PostId, Text, undefined).

create_comment(PostId, Text, Image) ->
  some_db:insert(comments, [{post_id, PostId}, {text, Text}, {image, Image}]).

update_comment(PostId, CommentId, Changes) ->
  some_db:update(comments, [{post_id, PostId}, {id, CommentId}], Changes).

update_comment(PostId, CommentId, Key, Value) ->
  update_comment(PostId, CommentId, [{Key, Value}]).

delete_comment(PostId, CommentId) ->
  some_db:delete(comments, [{post_id, PostId}, {id, CommentId}]).
```

*原因*: 上帝模块, 类似上帝对象, 了解过多或者负责过多的模块. 上帝模块通常是因为不断的增加功能函数演变出来的. A beautiful, to-the-point module with one job, one responsibility done well, gains a function. Then another, which does the same thing but with different parameters. 总有一天, 你会写出一个包含500多个函数并且高达6000多行代码的模块 .因此,让模块(和功能)只做一件事情就可以很容易地探索和理解代码,从而维护它.

***
#### 简洁的单元测试
> 只做一件事情也可以用到测试上. 当你在写 **单元** 测试时, 请保持简短,并且每个测试处理的事情不要超过2个.

*Examples*: [test_SUITE](src/test_SUITE.erl)

```erlang
bad(_Config) ->
  ct:comment("When input is 0, it should return 0"),
  0 = should:return(0),
  ct:comment("When input is positive, it should return 1"),
  1 = should:return(2),
  ct:comment("When input is negative, it should return -1"),
  -1 = should:return(-100),
  {comment, ""}.

good1(_Config) ->
  ct:comment("When input is 0, it should return 0"),
  0 = should:return(0),
  {comment, ""}.

good2(_Config) ->
  ct:comment("When input is positive, it should return 1"),
  1 = should:return(2),
  {comment, ""}.

good3(_Config) ->
  ct:comment("When input is negative, it should return -1"),
  -1 = should:return(-100),
  {comment, ""}.
```

*原因*: Multiple tests can identify multiple errors in one run, if you put all the things you want to test into one test you'll have to fix one thing at a time until the test passes.

***
#### Honor DRY
> Don't write the same code in many places, use functions and variables for that

*Examples*: [dry](src/dry.erl)

```erlang
%% @doc this is a very very trivial example, DRY has a much wider scope but it's
%%      provided just as an example
bad() ->
  case somthing:from(other, place) of
    {show, _} ->
      display:nicely(somthing:from(other, place));
    nothing ->
      display:nothing()
  end.

good() ->
  case somthing:from(other, place) of
    {show, _} = ThingToShow ->
      display:nicely(ThingToShow);
    dont_show_me ->
      display:nothing()
  end.
```

*原因*: This convention is specifically put in this list (instead of treat it as a [great idea](#great-ideas)) so that reviewers can reject PRs that include the same code several times or PRs that re-implement something that they know it's already done somewhere else.

***
#### 避免动态调用
> If there is no specific need for it, don't use dynamic function calling.

*Examples*: [dyn_calls](src/dyn_calls.erl)

```erlang
bad(Arg) ->
  Mods = [module_1, module_2, module_3],
  Fun = my_function,
  lists:foreach(
    fun(Mod) ->
      Mod:Fun(Arg)
    end, Mods).

good(Arg) ->
  mdoule_1:my_function(Arg),
  module_2:my_function(Arg),
  module_3:my_function(Arg).
```

*原因*: Dynamic calls can't be checked by [``xref``](http://erlang.org/doc/apps/tools/xref_chapter.html), one of the most useful tools in the Erlang world. ``xref`` is a cross reference checking/observing tool.

***
#### 使用文件夹将功能性相关的模块分组
> When having lots of modules, use subdirectories for them, named with a nice descriptive name for what that "package" does.

*原因*: That way it's easier to find what you need and determine what a certain module does.

*Note*: Remember to properly configure your ``Emakefile`` to handle that, if you use it.

***
#### 不要写面条式代码

> 不要写面条式代码(一个带 case 表达式的列表推导式, 或者用 begin/end 包含的代码块, 再嵌套结构...)

*Examples*: [spaghetti](src/spaghetti.erl)

```erlang
bad() ->
  Client = active_user:get_current_client(),
  [binary_to_list(Org)
   || Org <- autocomplete_db:members(
              case Client of
                home_client ->
                  <<"our:organizations">>;
                aperture_science ->
                  <<"client:", (prefix_for(aperture_science))/binary, ":orgs">>;
                wayne_ents ->
                  <<"client:", (prefix_for(wayne_ents))/binary, ":orgs">>
              end)].

good() ->
  Client = active_user:get_current_client(),
  RawOrgs = autocomplete_db:members(client_ac_key(Client)),
  [binary_to_list(Org) || Org <- RawOrgs].

client_ac_key(home_client) -> <<"our:organizations">>;
client_ac_key(Client) ->
  Prefix = prefix_for(Client),
  <<"client:", Prefix/binary, ":orgs">>.

prefix_for(aperture_science) -> <<"as">>;
prefix_for(wayne_ents) -> <<"we">>.
```

*原因*: 不要写面条式代码很难阅读, 理解和修改. The function callgraph for your program should strive to be a directed acyclic graph.

### 语法

Erlang语法很可怕, 我说得对吗? 所以你也可以充分利用它, 对吗? _对_?

***
#### 避免使用 if 表达式
> Don't use `if`.

*Examples*: [no_if](src/no_if.erl)

```erlang
bad(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  if
    Transport =/= cowboy_spdy, Version =:= 'HTTP/1.1' ->
      [{<<"connection">>, utils:atom_to_connection(Connection)}];
    true ->
      []
  end.


better(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  case {Transport, Version} of
    {cowboy_spdy, 'HTTP/1.1'} ->
      [{<<"connection">>, utils:atom_to_connection(Connection)}];
    {_, _} ->
      []
  end.
 

good(Connection) ->
  {Transport, Version} = other_place:get_http_params(),
  connection_headers(Transport, Version, Connection).
  
connection_headers(cowboy_spdy, 'HTTP/1.1', Connection) ->
    [{<<"connection">>, utils:atom_to_connection(Connection)}];
connection_headers(_, _, _) ->
    [].
```

*原因*: In some circumstances `if` introduces static boolean logic in your code, reducing code flexibility. In other cases, a `case` or a function call with pattern matching in its clauses is just more declarative. For newcommers (that have learned to use `if` in other languages), Erlang's `if` can be either hard to understand or easily abused.

*Debate*:
- [From OOP world](http://antiifcampaign.com/)
- [In this repo](issues/14)
- [In erlang-questions](http://erlang.org/pipermail/erlang-questions/2014-September/080827.html)

***
#### 避免嵌套 try...catches
> Don't nest `try…catch` clauses

*Examples*: [nested_try_catch](src/nested_try_catch.erl)

```erlang
bad() ->
  try
    maybe:throw(exception1),
    try
      maybe:throw(exception2),
      "We are safe!"
    catch
      _:exception2 ->
        "Oh, no! Exception #2"
    end
  catch
    _:exception1 -> "Bummer! Exception #1"
  end.

good1() ->
  try
    maybe:throw(exception1),
    maybe:throw(exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1";
    _:exception2 ->
      "Oh, no! Exception #2"
  end.

good2() ->
  try
    maybe:throw(exception1),
    a_function:that_deals(with, exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1"
  end.
```

*原因*: Nesting `try…catch` blocks defeats the whole purpose of them, which is to isolate the code that deals with error scenarios from the nice and shiny code that deals with the expected execution path.

### 命名

***
#### 在命名概念时保持一致
> 对于相同的概念，在任何地方都使用相同的变量名 (即使在不同的模块当中).

*Examples*: [consistency](src/consistency.erl)

```erlang
bad(UserId) -> internal_bad(UserId).

internal_bad(User_Id) -> internal_bad2(User_Id).

internal_bad2(Usr) -> db:get_by_id(Usr).


good(UserId) -> internal_good(UserId).

internal_good(UserId) -> internal_good2(UserId).

internal_good2(UserId) -> db:get_by_id(UserId).
```

*原因*: 当要找出所有用到``OrgID`` 的代码 (例如 我们想把变量从 ``string`` 转为 ``binary``), 我们只要搜索名为 ``OrgID``的变量，而不需要查找所有有可能关于 ``OrgID``的命名变量.

***
#### Explicit state should be explicitly named
> Name your state records ``#state`` and use ``-type state():: #state{}`` in all your OTP modules.

*Examples*: [state](src/state)

*原因*: OTP behaviours implementations usually require a state, and if it always have the same name it makes it more clearly recognizable. Defining a type for it, helps _dialyzer_ detect leaks (where an internal type as the state is used outside of the module).

***
#### Don't use _Ignored variables
> Variables beginning with _ are still variables, and are matched and bound, the _ just keeps the compiler from warning when you don't use them. If you add the _ to a variable's name, don't use it.

*Examples*: [ignored_vars](src/ignored_vars.erl)

```erlang
bad(_Number) -> 2 * _Number.

good(Number) -> 2 * Number.
```

*原因*: They are **not** supposed to be used.

***
#### 避免用布尔类型作为函数参数
> Don't use boolean parameters (i.e. `true` and `false`) to control clause selection.

*Examples*: [boolean_params](src/boolean_params.erl)

```erlang
bad(EdgeLength) -> bad_draw_square(EdgeLength, true).

bad_draw_square(EdgeLength, true) ->
  square:fill(square:draw(EdgeLength));
bad_draw_square(EdgeLength, false) ->
  square:draw(EdgeLength).

good(EdgeLength) -> good_draw_square(EdgeLength, full).

good_draw_square(EdgeLength, full) ->
  square:fill(square:draw(EdgeLength));
good_draw_square(EdgeLength, empty) ->
  square:draw(EdgeLength).
```

*原因*: Clarity of intention and not requiring the reader to check the function definition to understand what it does.

***
#### Stick to one convention for naming modules
> Stick to one convention when naming modules (i.e: ik_something vs iksomething vs something).

*Examples*: [naming_modules](src/naming_modules)

```shell
.
├── bad
│   ├── house.erl
│   └── xmpl_user.erl
└── good
    ├── xmpl_house.erl
    └── xmpl_user.erl
```

*原因*: It gives coherence to your system.

***
#### 原子(atoms)请用小写
> 原子命名只能使用小写字母. 当一个原子含有多个单词时,单词之间用 `_` 隔开. 特殊情况可以允许用大写 (例如  `'GET'`, `'POST'`, 等等) 但是尽量还是控制在一定使用量.

*Examples*: [atoms](src/atoms.erl)

```erlang
bad() -> ['BAD', alsoBad, bad_AS_well].

good() -> [good, also_good, 'good@its.mail'].
```

*原因*: Adhering to one convention makes it easier not to have "duplicated" atoms all around the code. Also, not using caps or special characters reduces the need for `'` around atoms.

***
#### 函数名
> Function names must use only lowercase characters or digits. Words in function names must be separated with `_`.

*Examples*: [function_names](src/function_names.erl)

```erlang
badFunction() -> {not_allowed, camel_case}.

'BAD_FUNCTION'() -> {not_allowed, upper_case}.

good_function() -> ok.

base64_encode() -> ok.
```

*原因*: Function names are atoms, they should follow the same rules that apply to them.

***
#### 变量名
> CamelCase must be used for variables. Don’t separate words in variables with `_`.

*Examples*: [variable_names](src/variable_names.erl)

```erlang
bad(Variablename, Another_Variable_Name) ->
  [Variablename, Another_Variable_Name].

good(Variable, VariableName) ->
  [Variable, VariableName].
```

*原因*: Adhering to one convention makes it easier not to have "duplicated" variables all around the code. Camel-case makes variable names more visually distinguishable from atoms and it matches the OTP standard.

### 字符串

***
#### IOLists over string concatenation
> Use iolists instead of string concatenation whenever possible

*Examples*: [iolists](src/iolists.erl)

```erlang
bad(Param) -> "Hello " ++ binary_to_list(Param) ++ "! Have a nice day!".

good(Param) -> ["Hello ", Param, "! Have a nice day!"].
```

*原因*: Performance and errors during conversion. [iolists](http://www.erlangpatterns.org/iolist.html) are just deeply nested lists of integers and binaries to represent IO data to avoid copying when concatenating strings or binaries.

### 宏

***
#### 宏的应用场景
> 除了包含以下使用方式的情况外，不要使用宏
> * 预定义部分: ``?MODULE``, ``?MODULE_STRING`` and ``?LINE``
> * 魔术数字: ``?DEFAULT_TIMEOUT``

*Examples*: [macros](src/macros.erl)
```erlang
-module(macros).

-define(OTHER_MODULE, other_module).
-define(LOG_ERROR(Error),
        error_logger:error_msg(
          "~p:~p >> Error: ~p~n\tStack: ~p",
          [?MODULE, ?LINE, Error, erlang:get_stacktrace()])).

-define(HTTP_CREATED, 201).

-export([bad/0, good/0]).

bad() ->
  try
    ?OTHER_MODULE:some_function(that, may, fail, 201)
  catch
    _:Error ->
      ?LOG_ERROR(Error)
  end.

good() ->
  try
    other_module:some_function(that, may, fail, ?HTTP_CREATED)
  catch
    _:Error ->
      log_error(?LINE, Error)
  end.

log_error(Line, Error) ->
  error_logger:error_msg(
    "~p:~p >> Error: ~p~n\tStack: ~p",
    [?MODULE, Line, Error, erlang:get_stacktrace()]).
```

*原因*: 宏的使用不利于调试工作的进行. 如果你尝试用它们来避免重复的代码块，可以使用以下函数去实现。
具体看 [related blog post](https://medium.com/@erszcz/when-not-to-use-macros-in-erlang-1d3f10d377f#.xc9b4bsl9) by [@erszcz](https://github.com/erszcz).

***
#### 宏名要大写
> 宏名应以大写字母命名:

*Examples*: [macro_names](src/macro_names.erl)
```erlang
-module(macro_names).

-define(bad, 1).
-define(BADMACRONAME, 2).
-define(Bad_Macro_Name, 3).
-define(Bad_L33t_M@Cr0, 4).

-define(GOOD, 5).
-define(GOOD_MACRO_NAME, 6).
```

*原因*: 这样做可以区分开普通变量和宏,在使用`grep`等工具查找这个宏时不会出现重复宏名,让查找变得更加容易等好处

***
#### 模块或函数名不能用宏命名
> 模块或函数名不能用宏命名

*Examples*: [macro_mod_names](src/macro_mod_names.erl)
```erlang
-module(macro_mod_names).

-define(SERVER, ?MODULE). % Oh, god! Why??
-define(TM, another_module).

-export([bad/1, good/1]).

bad(Arg) ->
  Parsed = gen_server:call(?SERVER, {parse, Arg}),
  ?TM:handle(Parsed).

good(Arg) ->
  Parsed = gen_server:call(?MODULE, {parse, Arg}),
  another_module:handle(Parsed).
```

*原因*: 当你需要复制这些代码到控制台调试时(这种情况出现的频率很高),你会发现这是一件很艰难的事情,因为我们必须手动地去替换掉所有的宏.

### 记录(Records)

***
#### 记录(`record`) 命名
> 记录(`record`)命名只能使用小写字母. 单词之间用 `_`分隔. 这个规则同样适用于`record`的字段名

*Examples*: [record_names](src/record_names.erl)

```erlang
-module(record_names).

-export([records/0]).

-record(badName, {}).
-record(bad_field_name, {badFieldName :: any()}).
-record('UPPERCASE', {'THIS_IS_BAD' :: any()}).

-record(good_name, {good_field_name :: any()}).

records() -> [#badName{}, #bad_field_name{}, #'UPPERCASE'{}, #good_name{}].
```

*原因*: `record`和其字段名都是原子(`atom`), 因此跟原子的命名规则是一样的.

***
#### 记录(`record`)先行
> 记录(`record`)在模块(Module)中使用的函数体之前先定义

*Examples*: [record_placement](src/record_placement.erl)

```erlang
-module(record_placement).

-export([good/0, bad/0]).

-record(good, { this_record   :: any()
              , appears       :: any()
              , before        :: any()
              , the_functions :: any()}).

good() -> [#good{}].

-record(bad,  { this_record :: any()
              , appears     :: any()
              , below       :: any()
              , a_function  :: any()}).

bad() -> [#bad{}].
```

*原因*: 记录(`record`)用于定义数据类型，这些数据类型很可能被模块中的多个函数所使用, 所以他们的定义不能只局限于一个. 此外，由于记录将与类型相关联，所以将它们以类似于文档的方式放在代码中是一种很好的做法 (把定义放在模块顶部).

***
#### 不要共享记录(`record`)
> 记录(`record`)不应该在多个模块之间共享. 如果你需要共享用record定义的对象, 使用不透明的导出类型，并在模块中提供转换函数接口.

*Examples*: [record_sharing](src/record_sharing.erl)

```erlang
-module(record_sharing).

-include("record_sharing.hrl").

-export([bad/0, good/0, good_field/1, good_field/2]).

-record(good, {good_field :: string()}).
-opaque good() :: #good{}.
-export_type([good/0]).

-spec good() -> good().
good() -> #good{}.

-spec good_field(good()) -> string().
good_field(#good{} = Good) -> Good#good.good_field.

-spec good_field(good(), string()) -> good().
good_field(#good{} = Good, Value) -> Good#good{good_field = Value}.

-spec bad() -> #bad{}.
bad() -> #bad{}.
```

*原因*: 记录(`record`)用于数据结构定义，隐藏这些结构有助于封装和抽象. 如果一个记录结构需要更改，它的定义在一个`.hrl`文件中，开发人员就需要找到所有引用该`.hrl`的文件，确认记录的改变并没有破坏任何已有的功能. 如果记录结构只是在它自己的模块的内部使用，那么就不需要做上述检查修改工作了.

***
#### 在 specs 里避免出现记录(`record`)
> 在 specs 里应该尽可能用 `types` 代替 记录(`records`).

*Examples*: [record_spec](src/record_spec.erl)

```erlang
-module(record_spec).

-record(state, {field1:: any(), field2:: any()}).

-opaque state() :: #state{}.

-export_type([state/0]).

-export([bad/1, good/1]).

-spec bad(#state{}) -> {any(), #state{}}.
bad(State) -> {State#state.field1, State}.

-spec good(state()) -> {any(), state()}.
good(State) -> {State#state.field1, State}.
```

*原因*: 类型可以导出使用,同时也有助于文档化, 使用 ``opaque`` 可以对记录进行封装和抽象.

***
####  记录(`record`)的类型(`Types`)
> 保持给记录(`record`)的每个字段添加类型定义的习惯

*Examples*: [record_types](src/record_types.erl)

```erlang
-module(record_types).

-export([records/0]).

-record(bad, {no_type}).

-record(good, {with_type :: string()}).

records() -> [#bad{}, #good{}].
```

*原因*: 记录(`record`)定义的是数据结构, 而其中最重要的部分之一就是记录组成部分的类型定义.

### 其它

***
#### Write function specs
> Write the **-spec**'s for your exported fun's, and for unexported fun's when it adds real value for documentation purposes. Define as many types as needed.

*Examples*: [specs](src/specs.erl)

*原因*: Dialyzer output is complicated as is, and it is improved with good type names. In general, having semantically loaded type names for arguments makes reasoning about possible type failures easier, as well as the function's purpose.

***
#### Use -callback attributes over behaviour_info/1.
> Unless you know your project will be compiled with R14 or lower, use ``-callback`` instead of ``behavior_info/1`` for your behavior definitions.

*Examples*: [callbacks](src/callbacks)

*原因*: Avoid deprecated functionality

***
#### Use atoms or tagged tuples for messages
> When sending a message between processes, you should typically either send a single, human-readable atom, or a tuple with a human-readable atom placed in element 1. This includes messages being sent via ``gen_server:call`` and the like.

*Examples*: [message-formatting](src/message_formatting.erl)

*原因*: Tagging messages with a distinctive, human-readable atom helps clarify the purpose of a message for anyone reading or debugging the code. Using element 1 of the tuple makes code more consistent and predictable, and improves readability when browsing through multiple clauses of functions like ``handle_call``.

This pattern also helps avoid bugs where different messages get confused with one another, or where messages get sent to the wrong recipient; it's much easier to find the source of an unexpected message if it looks like ``{set_foobar_worker_pid, <0.312.0>}`` than if you just find a bare pid in your mailbox.

***
#### No nested header inclusion
> When having many _nested_ "include files", use -ifndef(HEADER_FILE_HRL) .... -endif so they can be included in any order without conflicts.

*Examples*: [nested](include/nested.hrl)

*原因*: ``-include`` directives in included headers may lead to duplication of inclusions and/or other conflicts and it also hides things from the developer view.

***
#### 在头文件里不要有类型定义
> 在头文件里不要有 `-type` 

*Examples*: [types](src/types.erl)

*原因*: Defining types in public header files (especially those intended for inclusion via `-include_lib()`) might lead to type name clashes between projects and even modules of a single big project.
Instead, types should be defined in modules which they correspond to (with `-export_type()`) and this way take advantage of the namespacing offered by module names.
In other words, "no type definitions in header files" rule means that we will always need to use `some_mod:some_type()` unless referring to a type from the same module it's defined in.
Following this rule you also get the benefits that `-opaque` types provide, for instance, to dialyzer.

***
#### 不要用 import
> Do not use the `-import` directive

*Examples*: [import](src/import.erl)

*原因*: Importing functions from other modules makes the code harder to read and debug since you cannot directly distinguish local from external functions. In appropriately named functions, the module is _part_ of the function name, it gives meaning to it.

***
#### 不要用 export_all
> Do not use the `-compile(export_all)` directive

*Examples*: [export_all](src/export_all.erl)

*原因*: It's generally considered best to only export the specific functions that make up your module's known and documented external API. Keeping this list of functions small and consistent encourages good encapsulation and allows for more aggressive refactoring and internal improvements without altering the experience for those who make use of your module.

***

***
#### Encapsulate OTP server APIs
> Never do raw ``gen_server`` calls across module boundaries; the call should be encapsulated in an API function in the same module that implements the corresponding ``handle_call`` function. The same goes for other such OTP constructs (``gen_server`` casts, ``gen_fsm`` events, etc).

*Examples*: [otp_encapsulation](src/otp_encapsulation.erl)

*原因*: By sticking to this pattern of encapsulation, we make it _much_ easier to find out where calls/events might originate from.
Instead of having to search through the entire source tree for e.g. ``gen_server`` calls that look like they might send a certain message to a given process, we can just search for calls to the corresponding API function.
This makes it much easier to modify APIs, and also allows us to benefit more from Dialyzer's checks, assuming our API functions have appropriate type specs on them.
We can also change the underlying message format without disturbing any code outside of the module in question, and we can more easily avoid issues with RPC calls when running a mixed cluster.
With good encapsulation, you can even do things like convert a ``gen_server`` to a ``gen_fsm`` without any code changes beyond just the one module.

***
#### No debug calls
> Unless your project is meant to be run as an escript, there should be no `io:format` nor `ct:pal` calls in your production code (i.e. in the modules inside the `src` folder). Same rule applies for `lager` or `error_logger` calls if they're used just for debugging purposes during test stages.

*Examples*: [debug_calls](src/debug_calls.erl)

*原因*: Leaving unnecessary logs on production code impacts performance. It increases the processing time for the functions you're debugging and also consumes disk space if the logs are written to a file (as they usually are). Besides, more often than not the log messages are only understood in the context of the test or debugging round in which they were created, therefore the become useless pretty fast.

***
#### Don't Use Case Catch
> Don't capture errors with `case catch`, use `try ... of ... catch` instead.

*Examples*: [case-catch](src/case_catch.erl)

*原因*: `case catch ...` mixes good results with errors which is confusing. By
using `try ... of ... catch` the golden path is kept separate from the error
handling. 


### 工具

***
#### 锁定你的依赖
> 当你使用 rebar.config 或者 Erlang.mk 工具的时候, 请给依赖制定 tag 或者 commit, 而不是分支.

*Examples*:
- [erlang.mk](priv/Makefile)
- [rebar.config](priv/rebar.config)

*原因*: You don't want to be suddenly affected by a change in one of your dependencies. Once you've found the right version for you, stick to it until you *need* to change.

***
#### Loud errors
> Don't let errors and exceptions go unlogged. Even when you handle them, write a log line with the stack trace.

*Examples*: [loud_errors](src/loud_errors.erl)

*原因*: The idea is that somebody watching the logs has enough info to understand what's happening.

***
#### Properly use logging levels
> When using lager, use the different logging levels with the following meanings:

*Meanings*:
  * ``debug``: Very low-level info, that may cover your screen and don't let you type in it :P
  * ``info``: The system's life, in some detail. Things that happen usually, but not all the time. You should be able to use the console with acceptable interruptions in this level.
  * ``notice``: Meaningful things that are worth noticing, like the startup or termination of supervisors or important gen_servers, etc…
  * ``warning``: Handled errors, the system keeps working as usual, but something out of the ordinary happened
  * ``error``: Something bad and unexpected happen, usually an exception or error (**DO** log the **stack trace** here)
  * ``critical``: The system (or a part of it) crashed and somebody should be informed and take action about it
  * ``alert``: _There is no rule on when to use this level_
  * ``emergency``: _There is no rule on when to use this level_

#### Prefer the https protocol over others when specifying dependency URLs
> When specifying dependencies in erlang.mk Makefiles or rebar.config, prefer using the https protocol to download the dependency repository.

*Examples*: [makefile example](src/dependency_protocol/dep_protocol.makefile) [rebar example](src/dependency_protocol/dep_protocol.config)

*原因*: HTTPS is recommended by GitHub and is easier for CI.

* [Git on the Server - The Protocols](http://git-scm.com/book/ch4-1.html)
* [GitHub Official Recommendation](https://help.github.com/articles/which-remote-url-should-i-use/)
* [GitHub Protocol Comparison](https://gist.github.com/grawity/4392747#file-github-protocol-comparison-md)

## 好的建议和方法
当我们写代码时，应该考虑以下一些注意事项，但是不要引发PR拒绝，或者含糊到无法连贯执行。

***
### 优先使用高级函数而不是手写的递归方法
> 有时实现函数最好的方式是编写递归函数, 但是比较经常的写法是使用 fold函数 或者 列表推导式 会更加安全和可读性更高.

*Examples*: [alternatives to recursion](src/recursion.erl)

```erlang
-module(recursion).

-export([recurse/1, fold/1, map/1, comprehension/1]).

%%
%% 例子:
%% 不同的方法实现大写字符串
%%

%% 差的: 使用不必要的人工手写递归
recurse(S) ->
    lists:reverse(recurse(S, [])).

recurse([], Acc) ->
    Acc;
recurse([H | T], Acc) ->
    NewAcc = [string:to_upper(H) | Acc],
    recurse(T, NewAcc).

%% 好的: 使用fold函数实现同样的结果,更加安全，更少的代码行数
fold(S) ->
    Result = lists:foldl(fun fold_fun/2, [], S),
    lists:reverse(Result).

fold_fun(C, Acc) ->
    [string:to_upper(C) | Acc].

%% 更佳的: 使用map函数代替fold函数,更简单的实现方法，因为在这种情况下，fold函数大材小用了。
map(S) ->
    lists:map(fun string:to_upper/1, S).

%% 最好的: 在这种情况下，列表推导式最简单的实现方法（假设忽略string:to_upper也能直接对string使用的事实）
comprehension(S) ->
    [string:to_upper(C) || C <- S].
```

*原因*: 人工手写的递归容易出错, 并且代价昂贵。在有错误的情况下，一个错误的递归函数会失去它的基本功能，如螺旋般地失去控制，导致整个erlang节点挂掉，抵消了erlang最主要的好处之一： 进程的死亡不会导致整个节点的崩溃。

另外，对于一个有经验的erlang开发者而言，folds 和 列表推导式比复杂的递归函数更容易理解。显而易见的是:它们能为列表中的每个元素执行操作，递归也许同样能够实现，但是它经常需要仔细的检查，以验证控制流在实践中实际执行的路径。

***
### 驼峰式命名,下划线命名
> 符号命名：使用驼峰式命名变量，原子，函数和模块则使用下划线命名
> *Examples*: [camel_case](src/camel_case.erl)
```erlang
-module(camel_case).

-export([bad/0, good/0]).
%% 差的
bad() ->
  Variable_Name = moduleName:functionName(atomConstant),
  another_ModuleName:another_Function_Name(Variable_Name).
%% 好的
good() ->
  VariableName = module_name:function_name(atom_constant),
  another_module_name:another_function_name(VariableName).
```

*小节结论*:本节对下面一个问题很有帮助。

***
### 更短 (但仍保持有意义的) 的变量名称 

> 只要易于阅读和理解，保持变量名称简短。

*Examples*: [var_names](src/var_names.erl)
```erlang
-module(var_names).

-export([bad/1, good/1]).
%% 差的
bad(OrganizationToken) ->
  OID = organization:get_id(OrganizationToken),
  OID.
%% 好的
good(OrgToken) ->
  OrgID = organization:get_id(OrgToken),
  OrgID.
```

*小节结论*: 它有助于减少每行的长度，这也是上面描述的。

***
### 注释等级

> 模块注释用 **%%%**, 函数注释用 **%%**, 代码注释用 **%**.

*Examples*: [comment_levels](src/comment_levels.erl)

```erlang
% 这样的注释坏到家了
%%% @doc 这样的注释不错
-module(comment_levels).

-export([bad/0, good/0]).

% @doc 这样的注释不好
%%% @doc 这的注释也不好
bad() ->
  R = 1 + 2, %%% 这样的注释不好(not good)
  R. %% 这样的注释依然不好(bad again)

%% @doc 这种注释我喜欢
good() ->
  % 这个注释得到国际注释协会的一致认可
  % 还有 Chuck Norris的认可
  R = 1 + 2,
  R. % This comment (megusta)  这个注释我喜欢（megusta 西班牙语：我喜欢）
```
*小节结论*: 清晰的陈述了注释是什么, 并且寻找特定的注释比如："%% @"等 是非常有用的。

***
### 保持函数精简
> 只做一件事，尝试着用少量表达式来写函数. 除了集成测试外，每个函数理想的表达式数量是不超过**12**个.

*Examples*: [small_funs](src/small_funs.erl)

```erlang
-module(small_funs).

-export([bad/2, good/2]).
%% 差的
bad(UserEmail, Message) ->
  User =
    case users:find_by_email(UserEmail) of
      notfound ->
        users:new_with_email(UserEmail);
      FoundUser ->
        FoundUser
    end,
  
  EscapedMessage = message_utils:escape(Message),
  CleanMessage = bad_word_checker:clean(EscapedMessage),

  db:store_message(User, CleanMessage),
  
  DeviceIds = user:get_devices(User),
  lists:foreach(
    fun(DeviceId) ->
      case devices:get_device(DeviceId) of
        notfound -> ok;
        Device ->
          case device:get_push_info(Device) of
            {apns, Token} ->
              ApnsMsg = apns:build_message(CleanMessage),
              apns:send_msg(Token, ApnsMsg);
            {gcm, Token} ->
              GcmMsg = gcm:new_message(CleanMessage),
              gcm:send_message(Token, GcmMsg);
            _ -> ok
          end
      end
    end, DeviceIds).
%% 好的
good(UserEmail, Message) ->
  User = find_or_create_user(UserEmail),
  CleanMessage = clean_message(Message),

  db:store_message(User, CleanMessage),

  deliver_message(User, CleanMessage).


find_or_create_user(UserEmail) ->
  case users:find_by_email(UserEmail) of
    notfound ->
      users:new_with_email(UserEmail);
    FoundUser ->
      FoundUser
  end.

clean_message(Message) ->
  EscapedMessage = message_utils:escape(Message),
  bad_word_checker:clean(EscapedMessage).

deliver_message(User, Message) ->
  DeviceIds = user:get_devices(User),
  Devices =
    [devices:get_device(DeviceId) || DeviceId <- DeviceIds],
  lists:foreach(
    fun(notfound) -> ok;
       (Device) -> send_message(device:get_push_info(Device), Message)
    end, Devices).

send_message({apns, Token}, Message) ->
  ApnsMsg = apns:build_message(Message),
  apns:send_msg(Token, ApnsMsg);
send_message({gcm, Token}, Message) ->
  GcmMsg = gcm:new_message(Message),
  gcm:send_message(Token, GcmMsg);
send_message(_, _) -> ok.
```

*小结*: 从3个方面：
- 简洁的函数有助于是可读性和组装性。可读性又有助于维护。这一点强调的足够多了，你的代码越简洁，就越容易修复和更改。
- 简洁的函数目的清晰明了，因此您只需要了解执行操作的其中一小部分的子集，这使得验证它是否正确地工作变得非常简单。

- 强有力的论据：
  + 一个函数只干一件事情，如果函数太冗长你可能更适合改为以多个函数实现
  + 很明显，简单的，简洁的函数更容易理解
  + 重用性，保持函数的精简有利于后续使用（特别是erlang）
  + 屏幕尺寸：出于如何原因如果通过ssh连接服务器或者，你希望能够看到整个函数


*提示*:

本指导, 联合 **[避免多层嵌套](#avoid-deep-nesting)** and  **[在case表达式中使用更多更小的函数](#more-smaller-functions-over-case-expressions)** 两个指导, 可以很好的利用来构建函数，如下所示：

```erlang
some_fun(#state{name=foo} = State) ->
  do_foo_thing(),
  continue_some_fun(State);
some_fun(#state{name=bar} = State) ->
  do_bar_thing(),
  continue_some_fun(State).

continue_some_fun(State) ->
  ...,
  ok.
```

记住这些:

- 像这样在函数结尾调用函数是没有代价的
- 这种模式高效、紧凑、清晰
- 这样重置缩进，因此代码不会游离于屏幕右边边缘地带

最重要的:
- 测试起来简单，因为函数描绘了测试节点.
- 提供更多的跟踪切入面，因此我们能够找到哪里的代码计算运行导致脱轨，而嵌套case写法在运行时是不可跟踪的。

***
### 使用行为模式.

> 封装可重用的代码在行为模式里

*Examples*: [behavior](src/behavior.erl)
```erlang
-module(behavior).

-type element() :: binary().
-type id() :: pos_integer().

-export_type([element/0, id/0]).

-callback store(element()) -> id().
-callback retrieve(id()) -> notfound | element().
-callback delete(id()) -> ok.
-callback count() -> non_neg_integer().
```

*小结*: 这是OTP推进的方式

***
### 在发起的一方做防御编程

> 在你的代码的最外层进行校验

*Examples*: [validations](src/validations.erl)
```erlang
-module(validations).

-export([bad/1, good/1]).

bad(X) ->
  gen_server:call(?MODULE, {add, X}).

good(X) when is_integer(X) ->
  gen_server:call(?MODULE, {add, X});
good(X) ->
  throw({invalid_input, X}).
```

*小结*:一方面来讲你希望程序在哪里崩溃取决于你的API设计：在调用gen_server之前检查输入的参数将避免gen_server的整个往返调用，甚至错误的输入可能导致gen_server进程崩溃。
`do_it(Pid, X) when is_integer(X) -> gen_server:call(Pid, {do_it, X}).`如果你这样设计的话，参数错误时，调用方进程将崩溃。但是如果你不做这个函数头部匹配检查的话(指带`when is_integer(X)`)，将会导致gen_server进程崩溃。

***
### 避免不必要调用length/1 
> 许多用`length/1`作为`case`条件都可以被模式匹配替代掉，尤其在检查列表是否至少有一个元素时很管用。


*Examples*: [pattern matching](src/pattern_matching.erl)
```erlang
-module(pattern_matching).

-export([bad/1, good/1]).

bad(L) ->
  case length(L) of
    0 -> error;
    _ -> ok
  end.

good([]) ->
  error;
good(_L) ->
  ok.
```
*小结*:模式匹配是`Erlang`的核心内容之一，并且它的性能和可读性都很好。模式匹配也更加灵活，因此它使得代码逻辑变得更加简单。

***
### Move stuff to independent applications
> When you identify a block of functionality that is self-contained (it may be several modules or just a big one) and actually independent of the main purpose of your application, place that in a separate application. And consider open-sourcing it.

*原因*: It's easier to share among apps. If open-sourced, you're sharing it with the community and you get the benefits of the community being involved in it.

*Note*: Do **not** create highly specific libraries that are too coupled with the project you're working on. Use this rule for libraries that will likely be reused in other projects.

***
### Use the facade pattern on libraries
> [The facade pattern](http://en.wikipedia.org/wiki/Facade_pattern) is great to simplify library usage and serves as a form of self-documentation.

*Examples*: [kafkerl](https://github.com/inaka/kafkerl/blob/master/src/kafkerl.erl)

*原因*: Having the relevant functions in a single module means that the end user doesn't have a hard time figuring out which functions to call. Note that to avoid making it too complex, you probably want to carefully consider which functionality you wish to support here; exposing fewer functions (the ones that show the basic use of the library) as opposed to just creating a dummy module containing every single exported function in the library is prefered.
This greatly reduces the learning curve of the library and therefore makes it more tempting to use.

***
### Types in exported functions
> Custom data types used in exported functions should be defined with Erlang type declarations and exported from the module

*Examples*: [data_types](src/data_types.erl)

*原因*: It helps with function documentation and, when using opaque types, we ensure encapsulation.

***
### Separate responsibilities in sumo_db
> When using sumo_db you should separate the responsibilities clearly, creating for each entity:
> - one module (usually called MODELs) to describe the entity and allow administrating instances of the model in memory
> - one module (usually called MODEL_repo) to handle the various operations that require business logic relating to the entity

*Examples*: [separate responsibilities in sumo_db](https://github.com/inaka/fiar/tree/master/src/models)

*原因*: By dividing the functions into two different modules we increase understandability of the functionality especially if these are called from external modules. It also allows us to better organize the code and have smaller modules.
