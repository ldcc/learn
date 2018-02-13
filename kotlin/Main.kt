import train.testWith

fun main(args: Array<String>) {
//    println("Hello World")
//    val ldc = "ldc"
//    val person = Person(ldc)
//    var hello: String = "Hello "
//    hello += args[0]
//    println(hello)
//    val default = 0
//    println(args[default])
//    println({ args[0] })

//    val long = 0x2123123
//    println(message = long == 0x2123123)

//    println("$ldc.length is ${ldc.length}")
//    println({ ldc.length })
//    println(parseInt(null))
//    println(parseInt("123555"))
//    println(parseInt("ld12313"))

//    val myAny: Any = "Kotlin!"
//    println(getStringLength(myAny))

    /**
     * indices 返回一个 IntRange
     * indices 代表一个指数、区间或范围
     */
    //
//    for (i in args.indices) {
//        println(args[i])
//    }

    /**
     * 它等价与下面这种写法
     */
    //
//    val range: IntRange = IntRange(0, args.size)
//    val range: IntRange = 0..5
//    for (i in range) {
//        println(args[i])
//    }

    /**
     * for 循环还可以这么写
     */
    //
//    for (arg in args) {
//        println(arg)
//    }

    /**
     * while 循环
     */
    //
//    var i = 0
//    while (i < args.size) {
//        print(args[i++])
//    }

//    val x = args[2]
//    var y: Int?

    /**
     * 类型不相等或 x 为 null 都会导致抛出异常
     */
    //    y = x as Int

    /**
     * 类型不相等会导致抛出异常
     */
    //    y = x as Int?

    /**
     * 类型不相等或 x 为 null 都会返回 null
     */
    //    y = x as? Int

//    println(x)

//    val z: Int = 5
    /**
     * 使用 range
     *
     * z in 0..9
     * equals to
     * (0..9).contains(z)
     */
    //
//    if (z in 0..9) {
//        println(args.lastIndex)
//    }

    /**
     * z !in 0..4
     * equals to
     * !(z in 0..4)
     * equals to
     * (z in 0..4).not()
     */
    //
//    if ((0..4).contains(z).not()) {
//        println(args.size)
//    }

    /**
     * in 操作符
     * 相当于封装好的 contain() 函数
     */
    //
//    val name: String = ldc
//    if (name in args) {
//        println(ldc)
//    }

    /**
     * in 的另一个功能
     * in 在 for 中会变成一个迭代器
     */
    //
//    for (name in args) {
//        println(name)
//    }

//    val user = User(ldc, 19)
//    println(user.age)
//    val olderLdc = user.copy("older ldc")
//    println(olderLdc.toString())

    /**
     * 多重声明和函数默认值
     */
    //
//    val (name, age) = User("jane", 35)
//    println(foo(name, age))

    /**
     * 只读 List , Map
     */
    //
//    val list = listOf("a", "b", "c")
//    val map: Map<String, Int> = mapOf("a" to 1, "b" to 2, "c" to 3)
//    println(map[list[0]])

    /**
     * 懒加载 (请用 Debugger mode 打开)
     */
    //
//    val p: String by lazy { ldc }
//    val q: String = "q"
//    println(q)
//    println(p)

    /**
     * 如果使用 Kotlin 的单例类，其实是将整个类变成了静态类 (包括成员，方法) ，且不可调用构造函数
     */
    //
//    Resource()
//    println(Resource.name)

    /**
     * If not null and else 的简写
     */
    //
//    val files = File("test").listFiles()
    /**
     * 由于 size 没有指定类型，所以默认是 Any
     */
    //
//    val size = files?.size ?: "empty"
//    println(size)
    /**
     * ?: 还可以执行语句和代码块
     */
//
//    val file = files?.get(5) ?: throw IllegalStateException("file is missing")
//    file.let {
//        //        ...
//    }

//    NumPic.get()

    /**
     * with 使用
     */
    testWith()
}