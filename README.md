# これについて
Min-Caml (https://github.com/esumii/min-caml) 向けの自作コンパイラです。
(いちおうフルスクラッチですが、だいたいの大筋は元のやつに従っています)
## 詳細
Min-Camlのソースコードをnasm向けのアセンブリにコンパイルするコンパイラです。
可能なコンパイル先は、
- x86 Linux
- x86 Windows
- tortesia

です。 tortesia というのはCPU実験での自班のアーキテクチャです。
[tortesia2x86.py](https://github.com/satos---jp/cpuex_compiler/blob/master/scripts/tortesia2x86.py) をかますとnasm用のx86アセンブリになります。

# 使い方
例えばlinuxの場合、
```
$ (cd src; make)
$ ./scripts/compile_exec.sh ../tes.ml [ソースコード名]
```
とするとコンパイル → 実行ができます。
windowsの場合、 -w オプションをつけてコンパイルして、 elf32 を win32 にすればよいです。 

# 主な本家との変更点
- 授業課題のあれやこれが既に実装されている
- type_check や knorm や closure での抽象構文表現のためのヴァリアントをシンプルにした
- 各アーキ(といってもx86とtortesiaだけなのだけれど)の表現をvirtualまでまとめて、最後のemitで分けるようにした



