import re

tagDict = {'光甘草定调配液': ['甘草定', ''],
           '肌肽优化卸妆水300ml': ['肌肽优化卸妆水', ''],
           '75%酒精免洗抗菌液100ml（喷雾版）': ['75%酒精', '喷雾'],
           '75%酒精免洗抗菌液100ml（翻盖版）': ['75%酒精', '翻盖'],
           '75%酒精免洗抑菌洗手液（免洗型）60ml': ['75%酒精', '洗手液'],
           '酒精免洗抑菌洗手液80ml': ['酒精', '洗手液'],
           '神经酰胺水凝洁面泡泡': ['洁面泡泡', ''],
           '氨基酸水凝洁面乳': ['洁面乳', ''],
           '洋甘菊舒润精华乳液': ['洋甘菊', '精华'],
           '贻贝修护乳': ['贻贝', '乳'],
           '肌肽优化乳液': ['肌肽', '乳液'],
           '玫瑰滋养维E乳': ['维E乳', ''],
           '烟酰胺焕采弹弹乳': ['弹弹乳', ''],
           '洋甘菊舒润喷雾': ['洋甘菊', '喷雾'],
           '贻贝修护喷雾': ['贻贝', '喷雾'],
           '渗透精华水': ['渗透精华水', ''],
           '神经酰胺水凝修护水': ['神经酰胺', '修护水'],
           '透明质酸水光面膜': ['水光面膜', '赠'],
           '百花净颜补水巴布贴': ['巴布', ''],
           '甘草泛醇膜': ['甘草泛醇', ''],
           '净白补水祛斑霜': ['祛斑霜', '淡斑霜'],
           '贻贝修护霜': ['贻贝', '霜'],
           '鱼子胶原肌茵精华面霜': ['鱼子', ''],
           '寡肽红颜修护乳霜': ['寡肽红颜', ''],
           '肌肽焕亮精华霜': ['肌肽', '精华霜'],
           '净白补水祛斑霜（中样装）': ['中样', '赠'],
           '神经酰胺水凝精华液': ['神经酰胺', '精华液'],
           '神经酰胺水凝精华霜': ['神经酰胺', '精华霜'],
           '九肽雪肌精华液': ['雪肌', ''],
           '洋甘菊舒润原液': ['洋甘菊', '原液'],
           '红脸娃娃精华液': ['红脸', ''],
           '奇焕点亮精华液': ['奇焕', ''],
           '烟酰胺透肌熬夜精华液（1.5ml*7支）': ['熬夜', ''],
           '烟酰胺透肌熬夜精华液（体验装）': ['熬夜', '赠'],
           '透肌渗透精华液': ['渗透精华液', ''],
           '类人胶原蛋白敷料液（电商版）': ['类人', ''],
           '肌肽优化卸妆水100ml': ['卸妆水', '赠'],
           '纳米玻尿酸精华液+3%α熊果苷精华液组合': ['熊果苷', ''],
           '舒缓修护套盒': ['舒缓修护', ''],
           '纤连蛋白修护双效组合': ['纤连蛋白', ''],
           '蛋白酶焕肤修护套盒': ['蛋白酶', ''],
           '六胜肽密集修护柔肤水套盒': ['六胜肽', ''],
           '神经酰胺水凝修护3件套': ['神经酰胺', '3件套'],
           '净白祛斑VIP尊享组合': ['VIP', ''],
           '多功能美容黄金棒': ['黄金棒', ''],
           '美容补水仪': ['补水仪', ''],
           '贝玉压缩面膜（10粒装）': ['压缩面膜', ''],
           'BB亲肤化妆棉': ['化妆棉', ''],
           '至臻焕肤面部胶囊（精华油）新版': ['面部胶囊', ''],
           '红没药醇舒缓修护精华乳': ['红没药醇', '精华乳'],
           '红没药醇舒缓修护精华霜': ['红没药醇', '精华霜'],
           '海参修护冻干粉组合（10对）新版': ['海参', '冻干粉'],
           '活力修护精华液 (5对装) (新规格)': ['活力', ''],
           '寡肽水光焕亮冻干粉组合': ['寡肽', '冻干粉'],
           '（体验装）活力修护精华液（1对）': ['活力', '体验装'],
           '蛋白酶角质调理霜': ['蛋白酶角质', ''],
           '杜鹃花酸祛痘原液': ['杜鹃花', ''],
           '苦参祛痘原液': ['苦参', ''],
           '痘痕修护肽乳液': ['痘痕修护', ''],
           '东方黑水面膜': ['东方黑水', ''],
           '东方黑水面膜（单支装）': ['东方黑水', '单支装'],
           '红藻舒缓修红霜': ['红藻', ''],
           '角鲨烷修护精华油': ['角鲨烷', ''],
           '净颜修护双层卸妆液': ['卸妆液', ''],
           '多肽祛痘修印双效组合': ['多肽祛痘', ''],
           '九肽润亮面膜组合': ['九肽润亮', ''],
           '凝胶净透痘痘贴': ['痘痘贴', ''],
           '医用冷敷贴斑点皮肤护理型': ['冷敷贴', '斑点'],
           '医用冷敷贴晒后皮肤护理型': ['冷敷贴', '晒后'],
           '玻尿酸水光面膜': ['玻尿酸水光面膜', ''],
           '红没药醇舒缓修护精华水': ['红没药', '精华水']}


def checkTags(fullname):
    def check(tag):
        return tag == '' or re.search(tag, fullname) is not None
    return check


def checkProduct(iterable, pre):
    return all(map(pre, iterable))


def lookup(fullname):
    check = checkTags(fullname)
    for prod, tags in tagDict.items():
        if checkProduct(tags, check):
            return prod
    return None


if __name__ == '__main__':
    title = '敬修堂熬夜小安瓶精华 佰花方烟酰胺原液玻尿酸精华液7天补水提亮'
    result = lookup(title)
    print(result)
