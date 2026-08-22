/*
 * Игра 8885 — frontend.
 *
 * Подключается к ws://127.0.0.1:8000/ws, отображает динамику популяций
 * (ApexCharts) и гистограммы продолжительности жизни по видам, управляет
 * симуляцией (старт/пауза/шаг/рестарт/выбор правил).
 *
 * Протокол (кратко):
 *   сервер при подключении шлёт {"type":"hello",...} и {"type":"init",...};
 *   после каждого шага шлёт {"type":"state",...} с полем "lifespans" —
 *   гистограммой возраста смерти каждого вида (обновляется каждый ход);
 *   команды: {"type":"start"|"pause"|"step"|"restart"|"init"},
 *            {"type":"select","file":...}.
 *
 * Длинные истории: полные данные хранятся в state; для рендера строится
 * представление с децимацией — при числе шагов > MAX_POINTS берётся каждый
 * k-й шаг (k = ceil(n / MAX_POINTS)) плюс последний, чтобы график оставался
 * отзывчивым. Zoom/pan работают по этому представлению.
 */

'use strict';

const WS_URL = 'ws://127.0.0.1:8000/ws';
const MAX_POINTS = 3000;   // максимум точек на рендер до децимации
const RECONNECT_MS = 2000; // пауза перед переподключением

// ---------------------------------------------------------------------------
// Состояние клиента

const state = {
    species: [],      // [{ name, color }]
    steps: [],        // полный список шагов (из init)
    series: {},       // имя вида -> [численности]
    lifespans: {},    // имя вида -> [число умерших в возрасте 0, 1, ...]
    running: false,
    finished: false,
    result: null,
    free: 0,
    step: 0,
    ruleSet: '',
};

let ws = null;
let chart = null;
let histChart = null;
let ruleSelect, btnStart, btnStep, btnRestart, statusEl;

document.addEventListener('DOMContentLoaded', () => {
    ruleSelect = document.getElementById('rule-select');
    btnStart = document.getElementById('btn-start');
    btnStep = document.getElementById('btn-step');
    btnRestart = document.getElementById('btn-restart');
    statusEl = document.getElementById('status');

    ruleSelect.addEventListener('change', () => {
        send({ type: 'select', file: ruleSelect.value });
    });
    btnStart.addEventListener('click', () => {
        send(state.running ? { type: 'pause' } : { type: 'start' });
    });
    btnStep.addEventListener('click', () => send({ type: 'step' }));
    btnRestart.addEventListener('click', () => send({ type: 'restart' }));

    connect();
});

// ---------------------------------------------------------------------------
// WebSocket

function connect() {
    ws = new WebSocket(WS_URL);
    ws.onopen = () => setStatus('Соединение установлено…');
    ws.onmessage = (ev) => {
        let msg;
        try { msg = JSON.parse(ev.data); }
        catch (e) { return; }
        dispatch(msg);
    };
    ws.onclose = () => {
        setStatus('Потеряна связь с сервером, переподключение…');
        setTimeout(connect, RECONNECT_MS);
    };
    ws.onerror = () => { /* onclose сработает следом */ };
}

function send(obj) {
    if (ws && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify(obj));
    }
}

// ---------------------------------------------------------------------------
// Обработка сообщений сервера

function dispatch(msg) {
    switch (msg.type) {
        case 'hello':
            fillRuleSelect(msg.rules, msg.default);
            break;
        case 'init':
            applyInit(msg);
            break;
        case 'state':
            applyState(msg);
            break;
        case 'error':
            setStatus('Ошибка: ' + msg.message);
            break;
    }
}

function fillRuleSelect(rules, def) {
    ruleSelect.innerHTML = '';
    for (const r of rules) {
        const opt = document.createElement('option');
        opt.value = r;
        opt.textContent = r;
        if (r === def) opt.selected = true;
        ruleSelect.appendChild(opt);
    }
}

function applyInit(msg) {
    state.species = msg.species.map(([name, color]) => ({ name, color }));
    state.steps = msg.steps;
    state.series = {};
    for (const [name, counts] of Object.entries(msg.series)) {
        state.series[name] = counts.slice();
    }
    state.lifespans = {};
    for (const [name, arr] of Object.entries(msg.lifespans || {})) {
        state.lifespans[name] = arr.slice();
    }
    state.step = msg.step;
    state.free = msg.free;
    state.finished = msg.finished;
    state.result = msg.result;
    state.ruleSet = msg.ruleSet;
    state.running = !!msg.running; // сервер может продолжать автошаги после рестарта

    // держим селектор в соответствии с реально запущенным набором правил
    if (msg.ruleSet && ruleSelect) {
        for (const opt of ruleSelect.options) {
            if (opt.value === msg.ruleSet) { ruleSelect.value = msg.ruleSet; break; }
        }
    }

    updateChart();
    updateHist();
    updateControls();
    updateStatus();
}

// Численность вида из population-сообщения: принимает и объект
// {"Имя":n,...}, и массив пар [["Имя",n],...].
function popCount(pop, name) {
    if (!pop) return 0;
    if (Array.isArray(pop)) {
        for (const [n, c] of pop) if (n === name) return c;
        return 0;
    }
    return pop[name] || 0;
}

function applyState(msg) {
    // игнорируем сообщения, если график ещё не инициализирован
    if (state.species.length === 0) return;

    // при рестарте сервер шлёт init — если пришёл state со старым шагом, пропускаем
    if (msg.step < state.step && state.steps.length > 0) return;

    const prevStep = state.steps.length > 0 ? state.steps[state.steps.length - 1] : -1;
    if (msg.step > prevStep) {
        state.steps.push(msg.step);
        for (const sp of state.species) {
            state.series[sp.name].push(popCount(msg.population, sp.name));
        }
    }
    // гистограммы продолжительности жизни (обновляются каждый ход)
    if (msg.lifespans) {
        state.lifespans = {};
        for (const [name, arr] of Object.entries(msg.lifespans)) {
            state.lifespans[name] = arr.slice();
        }
    }
    state.step = msg.step;
    state.free = msg.free;
    state.finished = msg.finished;
    state.result = msg.result;
    state.running = !!msg.running;

    updateChart();
    updateHist();
    updateControls();
    updateStatus();
}

// ---------------------------------------------------------------------------
// Статус и кнопки

function updateStatus() {
    let text = 'Шаг: ' + state.step + ' | Свободно: ' + state.free;
    if (state.finished) {
        text += ' | Игра окончена: ' + (state.result || '—');
    }
    setStatus(text);
}

function setStatus(text) {
    if (statusEl) statusEl.textContent = text;
}

function updateControls() {
    btnStart.textContent = state.running ? 'Пауза' : 'Старт';
    btnStart.disabled = state.finished;
    btnStep.disabled = state.running || state.finished;
    btnRestart.disabled = false;
}

// ---------------------------------------------------------------------------
// График

// Представление серий для рендера (с децимацией длинных историй).
function buildView() {
    const n = state.steps.length;
    const k = n > MAX_POINTS ? Math.ceil(n / MAX_POINTS) : 1;
    const idx = [];
    for (let i = 0; i < n; i += k) idx.push(i);
    if (idx.length === 0 || idx[idx.length - 1] !== n - 1) idx.push(n - 1);

    return state.species.map(sp => ({
        name: sp.name,
        data: idx.map(i => ({ x: state.steps[i], y: state.series[sp.name][i] })),
    }));
}

function chartOptions() {
    const colors = state.species.map(sp => sp.color);
    return {
        series: buildView(),
        colors: colors,
        chart: {
            id: 'population',
            type: 'line',
            height: 560,
            fontFamily: 'Victor Mono',
            foreColor: '#d3d3d3',
            background: 'transparent',
            animations: { enabled: true, dynamicAnimation: { speed: 200 } },
            zoom: { enabled: true, type: 'x', autoScaleYaxis: false },
            pan: { enabled: true, type: 'x' },
            toolbar: {
                show: true,
                tools: {
                    download: false, selection: false,
                    zoom: true, zoomin: true, zoomout: true,
                    pan: true, reset: true,
                },
            },
        },
        dataLabels: { enabled: false },
        stroke: { curve: 'straight', width: 2 },
        markers: { size: 0 },
        grid: { borderColor: '#454545', strokeDashArray: 0 },
        xaxis: {
            type: 'numeric',
            title: { text: 'Шаг' },
            labels: { style: { colors: '#d3d3d3', fontSize: '12px' } },
            axisBorder: { color: '#454545' },
            axisTicks: { color: '#454545' },
        },
        yaxis: {
            min: 0,
            title: { text: 'Популяция' },
            labels: { style: { colors: '#d3d3d3', fontSize: '12px' } },
        },
        legend: {
            show: true,
            position: 'bottom',
            labels: { colors: '#d3d3d3' },
        },
        tooltip: { theme: 'dark' },
    };
}

function updateChart() {
    if (state.species.length === 0) return;
    const el = document.getElementById('chart');
    const series = buildView();
    if (!chart) {
        chart = new ApexCharts(el, chartOptions());
        chart.render();
    } else {
        chart.updateOptions({ colors: state.species.map(sp => sp.color) });
        chart.updateSeries(series);
    }
}

// ---------------------------------------------------------------------------
// Гистограмма продолжительности жизни (по видам)

// Гистограмма возраста смерти: для каждого вида столбики по возрастам
// 0, 1, 2, ... — сколько чибиков этого вида умерло в этом возрасте.
function buildHistView() {
    return state.species.map(sp => {
        const arr = state.lifespans[sp.name] || [];
        return {
            name: sp.name,
            data: arr.map((y, x) => ({ x, y })),
        };
    });
}

function histOptions() {
    const colors = state.species.map(sp => sp.color);
    return {
        series: buildHistView(),
        colors: colors,
        chart: {
            id: 'lifespans',
            type: 'bar',
            height: 320,
            stacked: false,
            fontFamily: 'Victor Mono',
            foreColor: '#d3d3d3',
            background: 'transparent',
            animations: { enabled: true, dynamicAnimation: { speed: 200 } },
            zoom: { enabled: false },
            toolbar: { show: false },
        },
        plotOptions: {
            bar: { columnWidth: '70%' },
        },
        dataLabels: { enabled: false },
        stroke: { width: 0 },
        grid: { borderColor: '#454545', strokeDashArray: 0 },
        xaxis: {
            type: 'numeric',
            title: { text: 'Возраст смерти (ходы)' },
            labels: { style: { colors: '#d3d3d3', fontSize: '12px' } },
            axisBorder: { color: '#454545' },
            axisTicks: { color: '#454545' },
        },
        yaxis: {
            min: 0,
            title: { text: 'Чибиков' },
            labels: { style: { colors: '#d3d3d3', fontSize: '12px' } },
        },
        legend: {
            show: true,
            position: 'bottom',
            labels: { colors: '#d3d3d3' },
        },
        tooltip: { theme: 'dark' },
    };
}

function updateHist() {
    if (state.species.length === 0) return;
    const el = document.getElementById('hist');
    if (!el) return;
    const series = buildHistView();
    if (!histChart) {
        histChart = new ApexCharts(el, histOptions());
        histChart.render();
    } else {
        histChart.updateOptions({ colors: state.species.map(sp => sp.color) });
        histChart.updateSeries(series);
    }
}
