#!/bin/bash
wget --quiet -O - http://mit.spbau.ru/sewiki/index.php/Unix_%D0%B8_%D0%A1%D0%BA%D1%80%D0%B8%D0%BF%D1%82%D0%BE%D0%B2%D1%8B%D0%B5_%D1%8F%D0%B7%D1%8B%D0%BA%D0%B8_2012 | grep -oE '[012][0-9]\.[01][0-9]\.20[0-9]{2}'
