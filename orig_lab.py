from functools import reduce
from sys import stdin, stdout
import os
import math
from matplotlib import pyplot as plt
import numpy as np

generative_functions = [
    ["sin(x)", lambda x: math.sin(x)],
    ["e^x", lambda x: math.e ** x],
    ["3x^3 - 2x^2 + 4", lambda x: 3 * x ** 3 - 2 * x ** 2 + 4]
]


def generate_points(searched_x):
    while True:
        print("Выберите функцию")
        for i, _ in enumerate(generative_functions):
            print(f"{i + 1}. {_[0]}")
        inp = int(input("Ваш выбор: "))
        if 0 >= inp or inp > len(generative_functions):
            print("Нет такой опции")
            continue
        actual_func = generative_functions[inp - 1][1]
        a = int(input("Введите нижнию границу отрезка: "))
        b = int(input("Введите верхнюю границу отрезка: "))
        if b <= a:
            print("Верхняя граница должна быть больше нижней")
            continue
        n = int(input("Введите количество точек: "))
        print(f"f(x)={actual_func(searched_x)}")
        h = (b - a) / (n - 1)
        x, y = [], []
        for i in range(n):
            x.append(a + i * h)
            y.append(actual_func(a + i * h))
        return x, y


def print_deltas(deltas, output_file):
    for i in range(len(deltas)):
        for _ in deltas[i]:
            print(f"{_:.4f}", end="\t", file=output_file)
        print(f"del{i}", file=output_file)


def lagrange_mnogochlen(xs, ys):
    n = len(xs)
    return lambda x: sum(
        [ys[i] * reduce(lambda a, b: a * b, [(x - xs[j]) / (xs[i] - xs[j]) if i != j else 1 for j in range(n)], 1) for i
         in range(n)])


def is_equidistant(xs):
    h = xs[1] - xs[0]
    flag = True
    for i in range(2, len(xs)):
        if not math.isclose(xs[i] - xs[i - 1], h):
            flag = False
    return flag


def newton_divided_differences(xs, ys):
    n = len(xs)
    coeffs = [ys[0] for i in range(n)]

    def find_func(a, b):
        poryadok = b - a
        if poryadok == 0:
            return ys[a]
        first = find_func(a + 1, b)
        second = find_func(a, b - 1)
        res = (first - second) / (xs[b] - xs[a])
        if a == 0:
            coeffs[poryadok] = res
        return res

    find_func(0, n - 1)
    return lambda x: coeffs[0] + sum(
        coeffs[k] * reduce(lambda a, b: a * b, [x - xs[j] for j in range(k)], 1) for k in range(1, n))


def newton_finite_differences(xs, ys, differences):
    def func(x):
        h = xs[1] - xs[0]
        mid = (xs[-1] + xs[0]) / 2
        if x <= mid:
            t = (x - xs[0]) / h
            return differences[0][0] + sum(
                [differences[i][0] * reduce(lambda a, b: a * b, [t - j for j in range(i)]) / math.factorial(i) for i in
                 range(1, len(differences))])
        else:
            t = (x - xs[-1]) / h
            return differences[0][-1] + sum(
                [differences[i][-1] * reduce(lambda a, b: a * b, [t + j for j in range(i)]) / math.factorial(i) for i in
                 range(1, len(differences))])

    return func


def stirling_mnogochlen(xs, ys, differences):
    def func(x):
        zero = len(xs) // 2
        h = xs[1] - xs[0]
        t = (x - xs[zero]) / h
        print(f"t для стирлинга: {t}")
        res = ys[zero]
        for i in range(1, zero + 1):
            loc_prod = t
            for j in range(1, i):
                loc_prod *= t ** 2 - j ** 2
            loc_prod /= math.factorial(2 * i - 1) * 2
            loc_prod *= differences[2 * i - 1][zero - i] + differences[2 * i - 1][zero - (i - 1)]
            res += loc_prod
            loc_prod = 1
            for j in range(i):
                loc_prod *= t ** 2 - j ** 2
            loc_prod /= math.factorial(2 * i)
            loc_prod *= differences[2 * i][zero - i]
            res += loc_prod
        return res
    return func


def bessel_mnogochlen(xs, ys, differences):
    def func(x):
        zero = len(xs) // 2 - 1
        n = zero + 1
        h = xs[1] - xs[0]
        t = (x - xs[zero]) / h
        res = 0
        for i in range(n):
            loc_prod = 1
            for j in range(-i, i):
                loc_prod *= t + j
            loc_prod /= math.factorial(2 * i) * 2
            loc_prod *= differences[2 * i][zero - i] + differences[2 * i][zero - (i - 1)]
            res += loc_prod
            loc_prod = t - 0.5
            for j in range(-i, i):
                loc_prod *= t + j
            loc_prod /= math.factorial(2 * i + 1)
            loc_prod *= differences[2 * i + 1][zero - i]
            res += loc_prod
        return res
    return func



def run(x, y, searched_x, output_file):
    deltas = [y]
    n = len(x)

    for i in range(1, n):
        deltas.append([])
        for j in range(n - i):
            deltas[i].append(deltas[i - 1][j + 1] - deltas[i - 1][j])
    print_deltas(deltas, output_file)

    func_lagrange = lagrange_mnogochlen(x, y)
    print(f"Интерполяция лагрнажа: {func_lagrange(searched_x)}")
    draw_plot(x, y, searched_x, func_lagrange, "Лагранж")

    func_newton_divided = newton_divided_differences(x, y)
    print(f"Интерполяция Ньютона (разд): {func_newton_divided(searched_x)}")
    draw_plot(x, y, searched_x, func_newton_divided, "Ньютон (разд)")
    if is_equidistant(x):
    # if True:
        func_newton_finite = newton_finite_differences(x, y, deltas)
        if searched_x <= (x[0] + x[-1]) / 2:
            print(f"Интерполяция Ньютона (кон, 1ая формула): {func_newton_finite(searched_x)}")
        else:
            print(f"Интерполяция Ньютона (кон, 2ая формула): {func_newton_finite(searched_x)}")
        draw_plot(x, y, searched_x, func_newton_finite, "Ньютон (кон)")

        if len(x) % 2 != 0:
            h = (x[1] - x[0])
            if abs((searched_x - x[len(x)//2]) / h) < 0.25:
                stirling_func = stirling_mnogochlen(x, y, deltas)
                print(f"Интерполяция Стирлинга: {stirling_func(searched_x)}")
                draw_plot(x, y, searched_x, stirling_func, "Стирлинг")
            else:
                new_x = x[-1] + h
                new_y = func_lagrange(new_x)
                print(f"ДОБАВИЛИ ТОЧКУ: ({new_x}; {new_y})")
                x.append(new_x)
                y.append(new_y)
                new_deltas = [y]
                n = len(x)

                for i in range(1, n):
                    new_deltas.append([])
                    for j in range(n - i):
                        new_deltas[i].append(new_deltas[i - 1][j + 1] - new_deltas[i - 1][j])
                bessel_func = bessel_mnogochlen(x, y, new_deltas)
                print(f"Интерполяция Бесселя: {bessel_func(searched_x)}")
                print_deltas(new_deltas, output_file)
                draw_plot(x, y, searched_x, bessel_func, "Бессель")

        else:
            bessel_func = bessel_mnogochlen(x, y, deltas)
            print(f"Интерполяция Бесселя: {bessel_func(searched_x)}")
            draw_plot(x, y, searched_x, bessel_func, "Бессель")



def draw_plot(xs, ys, searched_x, func, name):
    plt.clf()
    func_x = np.linspace(xs[0], xs[-1], 400)
    func_y = []
    for x in func_x:
        func_y.append(func(x))
    plt.plot(func_x, func_y, 'r')
    plt.scatter(xs, ys, color='b')
    plt.scatter([searched_x], [func(searched_x)], color='g')
    plt.title(name)
    plt.grid(True)
    plt.show()


def main():
    while True:
        try:
            X = []
            Y = []
            searched_x = float(input("Введите x для которого будем искать значения: "))
            print("Как будем находить чиселки?")
            print("1. С помощью ввода точек")
            print("2. Сгенерируем с помощью функции")
            inp = int(input("Ваш выбор: "))
            if inp == 1:
                inp_filename = input("Введите файл для считывания (ENTER для ручного ввода): ")
                if inp_filename == "":
                    print("Режим ручного ввода активирован.")
                    file = stdin
                else:
                    if not os.path.isfile(inp_filename):
                        raise FileNotFoundError(f"Файл {inp_filename} не найден.")
                    file = open(inp_filename, "r")
                for line in file:
                    if line.strip() == "":
                        continue
                    if "quit" in line:
                        break
                    numbers = line.strip().split()
                    if len(numbers) != 2:
                        raise ValueError(
                            f"Строка '{line.strip()}' содержит нечётное количество чисел. Каждая строка должна содержать пару чисел (x, y).")
                    X.append(float(numbers[0]))
                    Y.append(float(numbers[1]))
            elif inp == 2:
                X, Y = generate_points(searched_x)
            else:
                print("Нет такого варианта")
                continue

            out_filename = input("Введите файл для вывода (ENTER для вывода в консоль): ")
            if out_filename == "":
                out = stdout
            else:
                out = open(out_filename, "w+")

            run(X, Y, searched_x, out)
            out.close()
            break
        except FileNotFoundError as e:
            print("Файл не найден")
        except ValueError as e:
            print("Вводите чиселки")
        except Exception as e:
            print(f"Ошибка: {e}. Попробуйте снова.")


if __name__ == "__main__":
    main()