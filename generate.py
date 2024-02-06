import os
import io
import json
import requests
import random
from urllib.parse import quote
from tqdm.auto import tqdm
from typing import List, Dict, Any
import pandas as pd
import re
import string
from tenacity import retry, stop_after_attempt, wait_fixed

# This script is highly inspired by [Standoard's Alpaca](https://github.com/tatsu-lab/stanford_alpaca) and borrowed some functions from it.

# you can obtain the API key from [Google AI Studio](https://ai.google.dev/)
GOOGLE_AISTUDIO_API_KEY = os.getenv("GOOGLE_AISTUDIO_API_KEY")
NODRVPN_SERVICE_USERNAME = quote(os.getenv("NODRVPN_SERVICE_USERNAME"))
NODRVPN_SERVICE_PASSWORD = quote(os.getenv("NODRVPN_SERVICE_PASSWORD"))
proxy_servers = [
    "socks-us1.nordvpn.com",
    "socks-us10.nordvpn.com",
    "socks-us11.nordvpn.com",
    "socks-us12.nordvpn.com",
    "socks-us13.nordvpn.com",
    "socks-us14.nordvpn.com",
    "socks-us27.nordvpn.com",
    "socks-us28.nordvpn.com",
    "socks-us29.nordvpn.com",
    "socks-us3.nordvpn.com",
    "socks-us30.nordvpn.com",
    "socks-us31.nordvpn.com",
    "socks-us32.nordvpn.com",
    "socks-us33.nordvpn.com",
    "socks-us7.nordvpn.com",
    "socks-us8.nordvpn.com",
]
# here used NordVPN as the proxy service, you can use any other service by replace the following variables
proxy_server = proxy_servers[10] # random.choice(proxy_servers)
socks5_proxy = f"socks5://{NODRVPN_SERVICE_USERNAME}:{NODRVPN_SERVICE_PASSWORD}@{proxy_server}:1080"

hans_chars = set(
    "万与专业丛东丝丢两严丧个丬丰临为丽举么义乌乐乔习乡书买乱争于亏亘亚产亩亲亵亸亿仅从仑仓仪们价众优会伛伞伟传伤伥伦伧伪伫体佥侠侣侥侦侧侨侩侪侬俣俦俨俩俪俭债倾偬偻偾偿傥傧储傩儿兑兖党兰关兴兹养兽冁内冈册写军农冢冯冲决况冻净凄凉减凑凛凤凫凭凯击凼凿刍刘则刚创删别刬刭刽刿剀剂剐剑剥剧劝办务劢动励劲劳势勋勐勚匀匦匮区医华协单卖卢卤卫却卺厂厅历厉压厌厍厕厢厣厦厨厩厮县参叆叇双发变叙叠叶号叹叽吁吕吗吣吨听启吴呒呓呕呖呗员呙呛呜咏咙咛咝咤咴哌哑哒哓哔哕哗哙哜哝哟唛唝唠唡唢唣唤唿啧啬啭啮啰啴啸喷喽喾嗫嗳嘘嘤嘱噜嚣嚯团园囱围囵国图圆圣圹场坂坏块坚坛坜坝坞坟坠垄垅垆垒垦垧垩垫垭垯垱垲垴埘埙埚埝埯堑堕塆墙壮声壳壶壸处备复够头夸夹夺奁奂奋奖奥妆妇妈妩妪妫姗娄娅娆娇娈娱娲娴婳婴婵婶媪嫒嫔嫱嬷孙学孪宁宝实宠审宪宫宽宾寝对寻导寿将尔尘尧尴尸尽层屃屉届属屡屦屿岁岂岖岗岘岙岚岛岭岽岿峃峄峡峣峤峥峦崂崃崄崭嵘嵚嵛嵝嵴巅巩巯币帅师帏帐帘帜带帧帮帱帻帼幂幞并广庄庆庐庑库应庙庞废庼廪开异弃张弥弪弯弹强归当录彟彦彻径徕忆忏忧忾怀态怂怃怄怅怆怜总怼怿恋恳恶恸恹恺恻恼恽悦悫悬悭悯惊惧惨惩惫惬惭惮惯愍愠愤愦愿慑慭憷懑懒懔戆戋戏戗战戬户扦执扩扪扫扬扰抚抛抟抠抡抢护报担拟拢拣拥拦拧拨择挂挚挛挜挝挞挟挠挡挢挣挤挥挦捞损捡换捣据掳掴掷掸掺掼揽揿搀搁搂搅携摄摅摆摇摈摊撄撵撷撸撺擞攒敌敛数斋斓斩断无旧时旷旸昙昼昽显晋晒晓晔晕晖暂暧术机杀杂权条来杨杩极构枞枢枣枥枧枨枪枫枭柜柠柽栀栅标栈栉栊栋栌栎栏树栖样栾桊桠桡桢档桤桥桦桧桨桩梦梼梾检棂椁椟椠椤椭楼榄榇榈榉槚槛槟槠横樯樱橥橱橹橼檩欢欤欧歼殁殇残殒殓殚殡殴毁毂毕毙毡毵氇气氢氩氲汇汉汤汹沓沟没沣沤沥沦沧沨沩沪沵泞泪泶泷泸泺泻泼泽泾洁洒洼浃浅浆浇浈浉浊测浍济浏浐浑浒浓浔浕涛涝涞涟涠涡涢涣涤润涧涨涩淀渊渌渍渎渐渑渔渖渗温湾湿溃溅溆溇滗滚滞滟滠满滢滤滥滦滨滩滪漤潆潇潋潍潜潴澜濑濒灏灭灯灵灾灿炀炉炖炜炝点炼炽烁烂烃烛烟烦烧烨烩烫烬热焕焖焘煅煳熘爱爷牍牦牵牺犊犟状犷犸犹狈狍狝狞独狭狮狯狰狱狲猃猎猕猡猪猫猬献獭玑玙玚玛玮环现玱玺珉珏珐珑珰珲琎琏琐琼瑶瑷璎瓒瓮瓯电画畅畲畴疖疗疟疠疡疬疮疯疴痈痉痒痖痨痪痫瘅瘆瘗瘘瘪瘫瘾瘿癞癣癫癯皑皱皲盏盐监盖盗盘眍眦眬睁睐睑瞒瞩矫矶矾矿砀码砖砗砚砜砺砻砾础硁硕硖硗硙硚确硷碍碛碜碱碹磙礼祎祢祯祷祸禀禄禅离秃秆种积称秽秾稆税稣稳穑穷窃窍窑窜窝窥窦窭竖竞笃笋笔笕笺笼笾筑筚筛筜筝筹签简箓箦箧箨箩箪箫篑篓篮篱簖籁籴类籼粜粝粤粪粮糁糇紧絷纟纠纡红纣纤纥约级纨纩纪纫纬纭纮纯纰纱纲纳纴纵纶纷纸纹纺纻纼纽纾线绀绁绂练组绅细织终绉绊绋绌绍绎经绐绑绒结绔绕绖绗绘给绚绛络绝绞统绠绡绢绣绤绥绦继绨绩绪绫绬续绮绯绰绱绲绳维绵绶绷绸绹绺绻综绽绾绿缀缁缂缃缄缅缆缇缈缉缊缋缌缍缎缏缐缑缒缓缔缕编缗缘缙缚缛缜缝缞缟缠缡缢缣缤缥缦缧缨缩缪缫缬缭缮缯缰缱缲缳缴缵罂网罗罚罢罴羁羟羡翘翙翚耢耧耸耻聂聋职聍联聩聪肃肠肤肷肾肿胀胁胆胜胧胨胪胫胶脉脍脏脐脑脓脔脚脱脶脸腊腌腘腭腻腼腽腾膑臜舆舣舰舱舻艰艳艹艺节芈芗芜芦苁苇苈苋苌苍苎苏苘苹茎茏茑茔茕茧荆荐荙荚荛荜荞荟荠荡荣荤荥荦荧荨荩荪荫荬荭荮药莅莜莱莲莳莴莶获莸莹莺莼萚萝萤营萦萧萨葱蒇蒉蒋蒌蓝蓟蓠蓣蓥蓦蔷蔹蔺蔼蕲蕴薮藁藓虏虑虚虫虬虮虽虾虿蚀蚁蚂蚕蚝蚬蛊蛎蛏蛮蛰蛱蛲蛳蛴蜕蜗蜡蝇蝈蝉蝎蝼蝾螀螨蟏衅衔补衬衮袄袅袆袜袭袯装裆裈裢裣裤裥褛褴襁襕见观觃规觅视觇览觉觊觋觌觍觎觏觐觑觞触觯詟誉誊讠计订讣认讥讦讧讨让讪讫训议讯记讱讲讳讴讵讶讷许讹论讻讼讽设访诀证诂诃评诅识诇诈诉诊诋诌词诎诏诐译诒诓诔试诖诗诘诙诚诛诜话诞诟诠诡询诣诤该详诧诨诩诪诫诬语诮误诰诱诲诳说诵诶请诸诹诺读诼诽课诿谀谁谂调谄谅谆谇谈谊谋谌谍谎谏谐谑谒谓谔谕谖谗谘谙谚谛谜谝谞谟谠谡谢谣谤谥谦谧谨谩谪谫谬谭谮谯谰谱谲谳谴谵谶豮贝贞负贠贡财责贤败账货质贩贪贫贬购贮贯贰贱贲贳贴贵贶贷贸费贺贻贼贽贾贿赀赁赂赃资赅赆赇赈赉赊赋赌赍赎赏赐赑赒赓赔赕赖赗赘赙赚赛赜赝赞赟赠赡赢赣赪赵赶趋趱趸跃跄跖跞践跶跷跸跹跻踊踌踪踬踯蹑蹒蹰蹿躏躜躯车轧轨轩轪轫转轭轮软轰轱轲轳轴轵轶轷轸轹轺轻轼载轾轿辀辁辂较辄辅辆辇辈辉辊辋辌辍辎辏辐辑辒输辔辕辖辗辘辙辚辞辩辫边辽达迁过迈运还这进远违连迟迩迳迹适选逊递逦逻遗遥邓邝邬邮邹邺邻郄郏郐郑郓郦郧郸酝酦酱酽酾酿释鉴銮錾钆钇针钉钊钋钌钍钎钏钐钑钒钓钔钕钖钗钘钙钚钛钝钞钟钠钡钢钣钤钥钦钧钨钩钪钫钬钭钮钯钰钱钲钳钴钵钶钷钸钹钺钻钼钽钾钿铀铁铂铃铄铅铆铈铉铊铋铍铎铏铐铑铒铕铗铘铙铚铛铜铝铞铟铠铡铢铣铤铥铦铧铨铪铫铬铭铮铯铰铱铲铳铴铵银铷铸铹铺铻铼铽链铿销锁锂锃锄锅锆锇锈锉锊锋锌锍锎锏锐锑锒锓锔锕锖锗错锚锜锞锟锠锡锢锣锤锥锦锨锩锫锬锭键锯锰锱锲锳锴锵锶锷锸锹锺锻锼锽锾锿镀镁镂镃镆镇镈镉镊镌镍镎镏镐镑镒镕镖镗镙镚镛镜镝镞镟镠镡镢镣镤镥镦镧镨镩镪镫镬镭镮镯镰镱镲镳镴镶长门闩闪闫闬闭问闯闰闱闲闳间闵闶闷闸闹闺闻闼闽闾闿阀阁阂阃阄阅阆阇阈阉阊阋阌阍阎阏阐阑阒阓阔阕阖阗阘阙阚阛队阳阴阵阶际陆陇陈陉陕陧陨险随隐隶隽难雏雠雳雾霁霭靓静靥鞑鞒鞯鞴韦韧韨韩韪韫韬韵页顶顷顸项顺须顼顽顾顿颀颁颂颃预颅领颇颈颉颊颋颌颍颎颏颐频颒颓颔颕颖颗题颙颚颛颜额颞颟颠颡颢颣颤颥颦颧风飏飐飑飒飓飔飕飖飗飘飙飚飞飨餍饤饥饦饧饨饩饪饫饬饭饮饯饰饱饲饳饴饵饶饷饸饹饺饻饼饽饾饿馀馁馂馃馄馅馆馇馈馉馊馋馌馍馎馏馐馑馒馓馔馕马驭驮驯驰驱驲驳驴驵驶驷驸驹驺驻驼驽驾驿骀骁骂骃骄骅骆骇骈骉骊骋验骍骎骏骐骑骒骓骔骕骖骗骘骙骚骛骜骝骞骟骠骡骢骣骤骥骦骧髅髋髌鬓魇魉鱼鱽鱾鱿鲀鲁鲂鲄鲅鲆鲇鲈鲉鲊鲋鲌鲍鲎鲏鲐鲑鲒鲓鲔鲕鲖鲗鲘鲙鲚鲛鲜鲝鲞鲟鲠鲡鲢鲣鲤鲥鲦鲧鲨鲩鲪鲫鲬鲭鲮鲯鲰鲱鲲鲳鲴鲵鲶鲷鲸鲹鲺鲻鲼鲽鲾鲿鳀鳁鳂鳃鳄鳅鳆鳇鳈鳉鳊鳋鳌鳍鳎鳏鳐鳑鳒鳓鳔鳕鳖鳗鳘鳙鳛鳜鳝鳞鳟鳠鳡鳢鳣鸟鸠鸡鸢鸣鸤鸥鸦鸧鸨鸩鸪鸫鸬鸭鸮鸯鸰鸱鸲鸳鸴鸵鸶鸷鸸鸹鸺鸻鸼鸽鸾鸿鹀鹁鹂鹃鹄鹅鹆鹇鹈鹉鹊鹋鹌鹍鹎鹏鹐鹑鹒鹓鹔鹕鹖鹗鹘鹚鹛鹜鹝鹞鹟鹠鹡鹢鹣鹤鹥鹦鹧鹨鹩鹪鹫鹬鹭鹯鹰鹱鹲鹳鹴鹾麦麸黄黉黡黩黪黾鼋鼌鼍鼗鼹齄齐齑齿龀龁龂龃龄龅龆龇龈龉龊龋龌龙龚龛龟咨尝"
)
re_hans = r"|".join(list(hans_chars))


def count_hans_chars(text):
    return len([c for c in text if c in hans_chars])


@retry(stop=stop_after_attempt(3), wait=wait_fixed(1))
def count_token(prompt: str) -> int:
    api_url = f"https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:countTokens?key={GOOGLE_AISTUDIO_API_KEY}"

    res = requests.post(
        api_url,
        json={"contents": [{"parts": [{"text": prompt}]}]},
        proxies={"https": socks5_proxy, "http": socks5_proxy},
    )

    return res.json()["totalTokens"]


@retry(stop=stop_after_attempt(3), wait=wait_fixed(1))
def text_generation(prompt: str, stopSequences=[]) -> str:
    api_url = f"https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent?key={GOOGLE_AISTUDIO_API_KEY}"

    res = requests.post(
        api_url,
        json={
            "contents": [{"parts": [{"text": prompt}]}],
            "generationConfig": {
                "temperature": 0.9,
                "topK": 1,
                "topP": 1,
                "maxOutputTokens": 8192,  # maximum token limit
                "stopSequences": stopSequences,
            },
            "safetySettings": [
                {
                    "category": "HARM_CATEGORY_HARASSMENT",
                    "threshold": "BLOCK_NONE",  # we don't want to block any content
                },
                {"category": "HARM_CATEGORY_HATE_SPEECH", "threshold": "BLOCK_NONE"},
                {
                    "category": "HARM_CATEGORY_SEXUALLY_EXPLICIT",
                    "threshold": "BLOCK_NONE",
                },
                {
                    "category": "HARM_CATEGORY_DANGEROUS_CONTENT",
                    "threshold": "BLOCK_NONE",
                },
            ],
        },
        proxies={"https": socks5_proxy, "http": socks5_proxy},
    )

    return res.json()


def find_word_in_string(w, s):
    return re.compile(r"\b({0})\b".format(w), flags=re.IGNORECASE).search(s)


# modified from https://github.com/tatsu-lab/stanford_alpaca/blob/main/generate_instruction.py
def encode_prompt(prompt_instructions: List[Dict[str, Any]]):
    prompt = """You are asked to come up with a set of 20 diverse task instructions. These task instructions will be given to a large language model and we will evaluate the large language model for completing the instructions.

Here are the requirements:
1. Try not to repeat the verb for each instruction to maximize diversity.
2. The language used for the instruction also should be diverse. For example, you should combine questions with imperative instrucitons.
3. The type of instructions should be diverse. The list should include diverse types of tasks like open-ended generation, classification, editing, etc.
4. A large language model should be able to complete the instruction. For example, do not ask the assistant to create any visual or audio output. For another example, do not ask the assistant to wake you up at 5pm, a real-time information or set a reminder because it cannot perform any action.
5. The instructions and outputs should be in **Cantonese**.
6. The instructions should be 1 to 2 sentences long. Either an imperative sentence or a question is permitted.
7. You should generate an appropriate input to the instruction. The input field should contain a specific example provided for the instruction. It should involve realistic data and should not contain simple placeholders. The input should provide substantial content to make the instruction challenging but should ideally not exceed 100 words.
8. Not all instructions require input. For example, when a instruction asks about some general information, "世界上最高峰係乜嘢？", it is not necssary to provide a specific context. In this case, we simply put "<noinput>" in the input field.
9. The output should be an appropriate response to the instruction and the input. Make sure the output is less than 100 words.

List of 20 tasks:

"""

    """Encode multiple prompt instructions into a single string."""
    for idx, task_dict in enumerate(prompt_instructions):
        (instruction, input, output) = (
            task_dict["instruction"],
            task_dict["input"],
            task_dict["output"],
        )
        instruction = re.sub(r"\s+", " ", instruction).strip().rstrip(":")
        input = "<noinput>" if input.lower() == "" else input
        input = input.replace("<no input>", "<noinput>")
        prompt += f"###\n"
        prompt += f"{idx + 1}. Instruction: {instruction}\n"
        prompt += f"{idx + 1}. Input:\n{input}\n"
        prompt += f"{idx + 1}. Output:\n{output}\n"
    prompt += f"###\n"
    prompt += f"{idx + 2}. Instruction:"

    return prompt


def post_process_response(num_prompt_instructions, response: Dict[str, Any]):
    if (
        response is None
        or "candidates" not in response
        or len(response["candidates"]) == 0
    ):
        return []
    text = response["candidates"][0]["content"]["parts"][0]["text"]
    finish_reason = response["candidates"][0]["finishReason"]
    raw_instructions = f"{num_prompt_instructions+1}. Instruction:" + text
    raw_instructions = re.split("###", raw_instructions)
    instructions = []
    for idx, inst in enumerate(raw_instructions):
        # if the decoding stops due to length, the last example is likely truncated so we discard it
        if idx == len(raw_instructions) - 1 and finish_reason != "STOP":
            continue
        idx += num_prompt_instructions + 1
        inst = (
            inst.strip()
            .replace("Instruction：", "Instruction:")
            .replace("Input：", "Input:")
            .replace("Output：", "Output:")
        )
        splitted_data = re.split(f"{idx}\.\s+(Instruction|Input|Output):", inst)

        if len(splitted_data) != 7:
            print("error: invalid format", idx, len(splitted_data))
            continue
        else:
            inst = splitted_data[2].strip()
            input_txt = splitted_data[4].strip()
            input_txt = "" if input_txt.lower() == "<noinput>" else input_txt
            output = splitted_data[6].strip()

        # filter instruction or output containing Simplifed Chinese characters
        if re.search(re_hans, inst) or re.search(re_hans, output):
            print("error: contains Simplified Chinese", idx, len(list(inst)))
            continue

        # filter non-Cantonese output, this is a simple heuristic, not perfect, you can modify the threshold, in case you see there too many false positives
        if (
            count_hans_chars(inst) / len(inst) > 0.01
            or count_hans_chars(output) / len(output) > 0.01
        ):
            print("error: non-Cantonese output", idx, len(list(inst)))
            continue

        # filter out too short instructions and output
        if len(inst) <= 3 or len(output) <= 10:
            print("error: too short", idx, len(inst), len(output))
            continue

        # filter those starting with punctuation
        if inst[0] in string.punctuation:
            print("error: starting with punctuation", idx, len(list(inst)))
            continue
        # filter those starting with non-english character
        # if not inst[0].isascii():
        #     continue
        instructions.append({"instruction": inst, "input": input_txt, "output": output})
    return instructions


# modified from https://github.com/tatsu-lab/stanford_alpaca/blob/main/utils.py
def _make_r_io_base(f, mode: str):
    if not isinstance(f, io.IOBase):
        f = open(f, mode=mode)
    return f


def jload(f, mode="r"):
    """Load a .json file into a dictionary."""
    f = _make_r_io_base(f, mode)
    jdict = json.load(f)
    f.close()
    return jdict


def _make_w_io_base(f, mode: str):
    if not isinstance(f, io.IOBase):
        f_dirname = os.path.dirname(f)
        if f_dirname != "":
            os.makedirs(f_dirname, exist_ok=True)
        f = open(f, mode=mode)
    return f


def jdump(obj, f, mode="w", indent=4, default=str):
    """Dump a str or dictionary to a file in json format.

    Args:
        obj: An object to be written.
        f: A string path to the location on disk.
        mode: Mode for opening the file.
        indent: Indent for storing json dictionaries.
        default: A function to handle non-serializable entries; defaults to `str`.
    """
    f = _make_w_io_base(f, mode)
    if isinstance(obj, (dict, list)):
        json.dump(obj, f, indent=indent, default=default)
    elif isinstance(obj, str):
        f.write(obj)
    else:
        raise ValueError(f"Unexpected type: {type(obj)}")
    f.close()


if __name__ == "__main__":
    task_seeds_df = pd.read_json("data/seed.jsonl", lines=True)

    seed_instruction_data = [
        {
            "instruction": task_seeds_df["instruction"][i],
            "input": task_seeds_df["instances"][i][0]["input"],
            "output": task_seeds_df["instances"][i][0]["output"],
        }
        for i in range(len(task_seeds_df))
    ]

    if os.path.exists("data/regen.jsonl"):
        regen_df = pd.read_json("data/regen.jsonl", lines=True)

        print(f"Loaded {len(seed_instruction_data)} human-written seed instructions")

        regen_instruction_data = [
            {
                "instruction": regen_df["instruction"][i],
                "input": regen_df["input"][i],
                "output": regen_df["output"][i],
            }
            for i in range(len(regen_df))
        ]

        print(f"Loaded {len(regen_instruction_data)} regen instructions")

        seed_instruction_data += regen_instruction_data

    # Generate more instructions
    NUM_OF_GENERATED_INSTRUCTIONS = 1
    NUM_OF_SEED_INSTRUCTIONS = 3
    generated_instructions = []

    pbar = tqdm(total=NUM_OF_GENERATED_INSTRUCTIONS)

    pbar.update(len(generated_instructions))

    while True:
        instructions = random.choices(seed_instruction_data, k=NUM_OF_SEED_INSTRUCTIONS)
        prompt = encode_prompt(instructions)
        response = text_generation(prompt)
        output = post_process_response(NUM_OF_SEED_INSTRUCTIONS, response)

        generated_instructions.extend(output)

        if len(generated_instructions) >= NUM_OF_GENERATED_INSTRUCTIONS:
            break

        pbar.update(len(output))

    # export to jsonl and use it as a seed for the next round of generation
    pd.DataFrame(generated_instructions).to_json(
        "data/regen.jsonl", orient="records", lines=True, force_ascii=False
    )
