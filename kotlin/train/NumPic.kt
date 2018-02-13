package train

import java.awt.Color
import java.net.URL
import javax.imageio.ImageIO

object NumPic {
    private val PIC_URL1: String = "res/material/hj_1.png"
    private val PIC_URL2: String = "https://img.moegirl.org/common/5/5f/59da7494854d403f87236f481e75da86_%281%29.jpg"
    private val PIC_URL3: String = "http://www.cr173.com/up/2016-8/14707297921558622.jpg"
    private val PIC_URL4: String = "http://xinwen.jgaoxiao.com/Uploads/news/Attach/1456455577392573.jpg"

    fun get() {
//        val stream = FileImageInputStream(File(PIC_URL1))
//        val image = ImageIO.read(stream)
        val image = ImageIO.read(URL(PIC_URL4))
        val width = 0..image.width - 1
        val height = 0..image.height - 1
        (height step 3).forEach { y ->
            (width).forEach { x ->
                val _1 = Color(image.getRGB(x, y))
                val _3 = if (y + 1 in height) Color(image.getRGB(x, y + 1)) else _1
                val _5 = if (y + 2 in height) Color(image.getRGB(x, y + 1)) else _3
                val red = (_1.red + _3.red + _5.red) / 3
                val green = (_1.green + _3.green + _5.green) / 3
                val blue = (_1.blue + _3.blue + _5.blue) / 3
                print(when ((red + green + blue) / 3) {
                    in 0..20 -> ' '
                    in 21..95 -> '-'
                    in 96..130 -> '*'
                    else -> '0'
                })
            }
            println()
        }
    }
}
