# PHACKER : 一个超无趣、超庸俗但挺有用的STATA命令😘

<br>


<a href="http://github.com/haghish/github"><img src="https://cdn-us.imgs.moe/2023/06/12/6486999a9adb9.png" width="185px" height="188px"  align="left" hspace="10" vspace="6"></a>

 `phacker` 是一个Stata命令，它能够帮助你在堆积如山的变量中，筛选出回归后所有系数显著的所有变量组合。例如，你有一个因变量  `y` ，但有20个待筛选变量  `prevars`  ，本命令的使命就是在这20个待筛选变量的无数组合中，筛选出所有变量系数均显著且按R2高低排序的变量组合。此外，它允许你在待筛选变量外添加每次均参与回归的保留变量  `keepvar` ，保留变量也需要满足系数全显著的条件。**如果你的老板/上司/导师对P值有着狂热、诡异而痴迷的追求🤣🤣，不妨试试  `phacker` ！** 具体使用说明，可在安装命令后查询：

    help phacker

<br>

<br><img src="https://cdn-us.imgs.moe/2023/06/12/64869092798a2.png" align="left" width="250" hspace="10" vspace="6"> **致谢与说明：** `phacker` 的前半部分代码构建基于命令 `tuples` , 代码创作框架受到诸多关注系数显著性的命令的启发，在此向这些命令的作者们表示诚挚的感谢。感谢挚友 ***AuLu*** 同学，提供了一个如此美妙的契机，使我有机会写出 `phacker`。本命令目前还处于测试版, 在运行上可能会出现各种各样的问题，作者会不断优化。此外，受限于个人电脑的软硬件配置，将过多变量添加到本命令中可能会出现难以接受的灾难性后果。我建议添加到命令中的待筛选变量一般不应超过 **23** 个，即便是这样，得到结果所需的时间成本也可能是无比巨大的。

<br>

<img src="https://cdn-us.imgs.moe/2023/06/12/648691657cc41.jpg" width="50px" height="160px"  align="left" hspace="10" vspace="6"> 本页面更新于 **2023/06/12** ，欢迎交流👉 **yu.xiaokun@outlook.com**

<br>
<br>
