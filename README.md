# これについて
https://github.com/satos---jp/cpuex_compiler をSelf-Hostingしたいな～ 

# 使い方
src/compiler, src/lex, src/yacc で make をすると、それぞれ一番上に ./main, ./my_lex, ./my_yacc を出力します。それぞれ ocamlc, ocamllex, ocamlyacc に気持ち対応しています。
テストが test フォルダ内で ./tester.py を叩くと走ります。

# 進捗
lexer( https://github.com/satos---jp/mincaml_self_hosting/tree/master/src/lex )に関してbootstrapが(細かな部分を除いて)できた。(ハズ)

# mincamlからの拡張
- let多相
- ヴァリアント定義
- パターンマッチ
- String型,Ref型
- Openでのソースコード分割
- lexの実装
- yaccの実装
- 各種ライブラリ(List,String,Lexing,Parsing)の一部

# TODO
- Openまわりもっとシンプルにする
- Printfの型推論まわりちゃんとやる
- ガベージコレクション
- yaccの precedence
- 多相の値制限(なので現在型推論がボロボロ)
- Sysなどのモジュール追加
- try with
