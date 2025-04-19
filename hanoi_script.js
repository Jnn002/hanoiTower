const API_BASE_URL = "/api";
const pegA = document.getElementById("peg-a");
const pegB = document.getElementById("peg-b");
const pegC = document.getElementById("peg-c");
const pegs = [pegA, pegB, pegC];
const moveCountSpan = document.getElementById("move-count");
const minMovesSpan = document.getElementById("min-moves");
const statusMessageP = document.getElementById("status-message");
const resetButton = document.getElementById("reset-button");
const numDisksInput = document.getElementById("num-disks");

let selectedPeg = null;
let gameState = null;

async function fetchGameState() {
    try {
        const response = await fetch(`${API_BASE_URL}/state`);
        if (!response.ok) {
            throw new Error(`Error HTTP: ${response.status}`);
        }
        const data = await response.json();
        gameState = data;
        drawGame(gameState);
        updateMessages("", "status");
    } catch (error) {
        console.error("Error al obtener estado:", error);
        updateMessages("Error al conectar con el servidor.", "error");
    }
}

async function sendMove(fromPegName, toPegName) {
    try {
        const response = await fetch(`${API_BASE_URL}/move`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify({ from: fromPegName, to: toPegName })
        });
        const result = await response.json();

        gameState = result.state;
        drawGame(gameState);

        if (result.success) {
            updateMessages(result.message, "success");
            if (result.win) {
                updateMessages(
                    `¡Felicidades! Resuelto en ${gameState.moves} movimientos.`, //moves needed to complete the game
                    "win"
                );
            }
        } else {
            updateMessages(`Error: ${result.message}`, "error");
        }
    } catch (error) {
        console.error("Error al enviar movimiento:", error);
        updateMessages("Error de comunicación al mover.", "error");
    }
}

async function resetGame() {
    const numDisks = parseInt(numDisksInput.value, 10);
    if (isNaN(numDisks) || numDisks < 1) {
        updateMessages("Número de discos inválido", "error");
        return;
    }

    try {
        const response = await fetch(`${API_BASE_URL}/reset`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json"
            },
            body: JSON.stringify({ disks: numDisks })
        });
        const result = await response.json();
        if (result.success) {
            gameState = result.state;
            drawGame(gameState);
            updateMessages("Juego reiniciado.", "status");
        } else {
            updateMessages(`Error al reiniciar: ${result.message}`, "error");
        }
    } catch (error) {
        console.error("Error al reiniciar:", error);
        updateMessages("Error de comunicación al reiniciar.", "error");
    }
    clearSelection();
}

function drawGame(state) {
    if (!state) return;

    pegs.forEach(peg => (peg.innerHTML = ""));

    drawDisks(pegA, state.a);
    drawDisks(pegB, state.b);
    drawDisks(pegC, state.c);

    moveCountSpan.textContent = state.moves;
    minMovesSpan.textContent = state.min_moves;
}

function drawDisks(pegElement, diskList) {
    diskList.forEach((diskNumber, index) => {
        const diskDiv = document.createElement("div");
        diskDiv.classList.add("disk", `disk-${diskNumber}`);

        diskDiv.textContent = diskNumber;

        diskDiv.style.opacity = "0";
        diskDiv.style.transform = "scale(0.9)";

        pegElement.appendChild(diskDiv);

        setTimeout(() => {
            diskDiv.style.transition = "opacity 0.3s ease, transform 0.3s ease";
            diskDiv.style.opacity = "1";
            diskDiv.style.transform = "scale(1)";
        }, index * 50);
    });
}

function updateMessages(message, type) {
    statusMessageP.textContent = message;
    statusMessageP.className = type;
}

function handlePegClick(event) {
    const clickedPeg = event.currentTarget;

    // Efecto visual de clic en clavija
    clickedPeg.style.transform = "scale(0.98)";
    setTimeout(() => {
        clickedPeg.style.transform = "scale(1)";
    }, 150);

    if (!selectedPeg) {
        const pegName = clickedPeg.dataset.pegname;
        if (gameState && gameState[pegName] && gameState[pegName].length === 0) {
            showTooltip(clickedPeg, "No puedes mover desde una clavija vacía");
            updateMessages("No puedes mover desde una clavija vacía.", "error");
            return;
        }

        selectedPeg = clickedPeg;
        selectedPeg.classList.add("selected");
        if (selectedPeg.querySelector(".disk")) {
            const topDisk = selectedPeg.querySelector(".disk");
            topDisk.style.boxShadow = "0 0 10px rgba(255,215,0,0.8)";
            topDisk.style.transform = "translateY(-5px)";
        }
        updateMessages("Clavija origen seleccionada. Elige destino.", "status");
    } else if (selectedPeg === clickedPeg) {
        clearSelection();
        updateMessages("Selección cancelada.", "status");
    } else {
        const fromPegName = selectedPeg.dataset.pegname;
        const toPegName = clickedPeg.dataset.pegname;

        const topDisk = selectedPeg.querySelector(".disk");
        if (topDisk) {
            topDisk.style.boxShadow = "";
        }

        selectedPeg.classList.remove("selected");
        sendMove(fromPegName, toPegName);
        selectedPeg = null;
    }
}

function showTooltip(element, message) {
    const tooltip = document.createElement("div");
    tooltip.textContent = message;
    tooltip.style.position = "absolute";
    tooltip.style.backgroundColor = "rgba(0,0,0,0.7)";
    tooltip.style.color = "white";
    tooltip.style.padding = "5px 10px";
    tooltip.style.borderRadius = "5px";
    tooltip.style.fontSize = "14px";
    tooltip.style.zIndex = "1000";
    tooltip.style.top = "-30px";
    tooltip.style.left = "50%";
    tooltip.style.transform = "translateX(-50%)";
    tooltip.style.whiteSpace = "nowrap";
    tooltip.style.opacity = "0";
    tooltip.style.transition = "opacity 0.2s";

    element.style.position = "relative";
    element.appendChild(tooltip);

    setTimeout(() => {
        tooltip.style.opacity = "1";
    }, 10);

    setTimeout(() => {
        tooltip.style.opacity = "0";
        setTimeout(() => {
            element.removeChild(tooltip);
        }, 200);
    }, 2000);
}

function clearSelection() {
    if (selectedPeg) {
        const topDisk = selectedPeg.querySelector(".disk");
        if (topDisk) {
            topDisk.style.boxShadow = "";
            topDisk.style.transform = "";
        }

        selectedPeg.classList.remove("selected");
        selectedPeg = null;
    }
}

pegs.forEach(peg => peg.addEventListener("click", handlePegClick));
resetButton.addEventListener("click", resetGame);

fetchGameState();

document.addEventListener("DOMContentLoaded", function () {
    const resetBtn = document.getElementById("reset-button");

    resetBtn.addEventListener("mouseover", function () {
        this.style.backgroundColor = "#45a049";
    });

    resetBtn.addEventListener("mouseout", function () {
        this.style.backgroundColor = "#4CAF50";
    });

    resetBtn.addEventListener("mousedown", function () {
        this.style.transform = "scale(0.95)";
    });

    resetBtn.addEventListener("mouseup", function () {
        this.style.transform = "scale(1)";
    });
});
