from __future__ import annotations
from typing import Generic, TypeVar
TRule = TypeVar("TRule", bound="Rule")


class Rule(Generic[TRule]):
    psL: str = ''  # правая строка правила
    psR: str = ''  # левая строка правила
    isFinal: bool = False  # признак завершающего правила
    next: TRule | None = None  # следующее правило


st: str = ''  # строка обработки
rs: TRule | None = None  # схема правил (ссылка на первое)


'''
Считаем спецификацию из указанного файла-источника
Исключим пробелы и комментарии из правил
! Последняя строка не должна содержать лишних символов
'''
def readAlg(filename: str):
    global rs
    global st
    pr: TRule | None = None  # последнее правило в списке
    p: int  # строчный указатель

    with open(filename, 'r') as f:

        lines = f.readlines()
        last = lines[-1]

        for line in lines:

            if line is last:
                st = line
                continue

            line = line[:line.find('//')]
            line = line.replace(' ', '')

            p = line.find('->')

            if p == -1: continue

            if rs is None:
                rs = Rule()
                pr = rs
            else:
                pr.next = Rule()
                pr = pr.next

            if line.find('x') != -1:
                pr.isFinal = True
                line.replace('x', '')

            pr.psR = line[(p + 2):]
            pr.psL = line[:p]


'''
Выведем спецификацию на печать
'''
def printAlg():
    global rs
    global st
    pr: TRule | None = rs

    print('начальная строка:', st)

    while pr is not None:

        print(pr.psL, '->', pr.psR)

        if pr.isFinal is True:
            print('x')

        pr = pr.next

    return 0


'''
Просмотрим правила по порядку
Сделаем замену найденной подстроки
Восстановим очередность, если найдена возможность замены
'''
def execute():
    global rs
    global st
    pr: TRule | None = rs  # проверяемое правило
    p: int  # строчный указатель
    i: int = 1  # счетчик применяемого правила

    while pr is not None:

        p = st.find(pr.psL)

        if p != -1:

            print(pr.psL, '-', i, '->', pr.psR)
            st = st.replace(pr.psL, pr.psR, 1)

            if len(st) > 255:
                print('*** overflow ***')
                break

            if pr.isFinal:
                print('*** final rule ***')
                break

            print(st)
            pr = rs
            i = 1

        else:

            pr = pr.next
            i += 1

    print('*** no more rules ***')
    print('итоговая строка:', st)

    return 0


if __name__ == '__main__':
    readAlg('alg_redund.txt')
    printAlg()
    execute()
