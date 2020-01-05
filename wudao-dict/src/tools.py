# global functions
from urllib.request import urlopen
from urllib.parse import urlparse
from urllib.parse import quote
import os
import urllib.error

mon_ip = '119.28.128.77'

def get_ip():
    if ie():
        try:
            res = urlopen('https://chestnutheng.cn/IP', timeout=1)
            xml = res.read().decode('utf-8')
            with open('./IP', 'w') as f:
                f.write(xml)
            return xml.strip()
        except:
            if os.path.exists('./IP'):
                with open('./IP', 'r') as f:
                    return f.read().strip()
            else:
                return mon_ip
    else:
        if os.path.exists('./IP'):
            with open('./IP', 'r') as f:
                return f.read().strip()
        else:
            return mon_ip
            
    
def ie():
    try:
        urlopen('http://www.baidu.com', timeout=1)
        return True
    except urllib.error.URLError as err: 
        return False

def report_new_word(x, ip):
    x = quote(x)
    url = urlparse('https://' + ip + '/wudao/add_new_word/' + x)
    res = urlopen(url.geturl(), timeout=1)
    xml = res.read().decode('utf-8')
    return xml
    
def report_old_word(x, ip):
    x = quote(x)
    url = urlparse('https://' + ip + '/wudao/add_old_word/' + x)
    res = urlopen(url.geturl(), timeout=1)
    xml = res.read().decode('utf-8')
    return xml
    
def is_alphabet(uchar):
    if (u'\u0041' <= uchar <= u'\u005a') or \
            (u'\u0061' <= uchar <= u'\u007a') or uchar == '\'':
        return True
    else:
        return False
        
        
