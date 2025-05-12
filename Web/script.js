const traducciones = {
  es: {
    titulo: "🕵️ Detector de Fake News",
    analizar: "Analizar",
    resultado: "Resultado del Análisis",
    veredicto: "Veredicto:",
    puntuacion: "Puntuación:",
    palabras: "Palabras clave:",
    resumen: "Resumen del texto:",
    relacionadas: "Noticias relacionadas:",
    sinRelacionadas: "No se encontraron noticias relacionadas."
  },
  en: {
    titulo: "🕵️ Fake News Detector",
    analizar: "Analyze",
    resultado: "Analysis Result",
    veredicto: "Verdict:",
    puntuacion: "Score:",
    palabras: "Keywords:",
    resumen: "Text Summary:",
    relacionadas: "Related News:",
    sinRelacionadas: "No related news found."
  }
};

const idioma = navigator.language.startsWith("es") ? "es" : "en";

// Traducir etiquetas
document.querySelectorAll("[data-i18n]").forEach(el => {
  const key = el.getAttribute("data-i18n");
  el.textContent = traducciones[idioma][key] || el.textContent;
});

document.getElementById("formulario").addEventListener("submit", async function (e) {
  e.preventDefault();

  const url = document.getElementById("url").value;
  const resultado = document.getElementById("resultado");
  const contRel = document.getElementById("relacionadas");
  const cargando = document.getElementById("cargando");

  resultado.classList.add("oculto");
  contRel.innerHTML = "";
  cargando.classList.remove("oculto");

  try {
    const res = await fetch("http://localhost:8000/analizar_url", {
      method: "POST",
      headers: { "Content-Type": "application/x-www-form-urlencoded" },
      body: new URLSearchParams({ url })
    });

    const data = await res.json();
    console.log("🧪 Respuesta del backend:", data);
    cargando.classList.add("oculto");

    if (data.error) {
      alert("⚠️ " + data.error);
      return;
    }

    document.getElementById("veredicto").innerText = data.veredicto;
    document.getElementById("puntuacion").innerText = data.puntuacion;
    document.getElementById("palabrasClave").innerText = data.palabrasClave.join(", ");
    document.getElementById("resumen").innerText = data.resumen;

    const h3 = document.createElement("h3");
    h3.textContent = traducciones[idioma].relacionadas;
    contRel.appendChild(h3);

    if (data.relacionadas && data.relacionadas.length > 0) {
      data.relacionadas.forEach(n => {
        const link = document.createElement("a");
        link.href = n.url;
        link.target = "_blank";
        link.textContent = `📰 ${n.titulo} (${n.fuente})`;
        contRel.appendChild(link);
        contRel.appendChild(document.createElement("br"));
      });
    } else {
      const mensaje = document.createElement("p");
      mensaje.textContent = traducciones[idioma].sinRelacionadas;
      contRel.appendChild(mensaje);
    }

    resultado.classList.remove("oculto");
  } catch (error) {
    cargando.classList.add("oculto");
    console.error("❌ Error al conectar con la API:", error);
    alert("❌ Error al conectar con la API.");
  }
});
