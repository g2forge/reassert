package com.g2forge.reassert.maven.modifier;

import java.nio.file.Path;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class AXMLTransformMavenPOMModifier implements IMavenPOMModifier {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final DocumentBuilderFactory documentBuilderFactory = computeDocumentBuilderFactory();

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final TransformerFactory transformFactory = computeTransformerFactory();

	protected static DocumentBuilderFactory computeDocumentBuilderFactory() {
		return DocumentBuilderFactory.newInstance();
	}

	protected static TransformerFactory computeTransformerFactory() {
		return TransformerFactory.newInstance();
	}

	@Override
	public void accept(Path input, Path output) {
		try {
			final Document document = createDocumentBuilder().parse(input.toFile());
			final Transformer transformer = createTransformer();

			final DOMSource source = new DOMSource(document);
			final StreamResult result = new StreamResult(output.toFile());
			transformer.transform(source, result);
		} catch (Throwable throwable) {
			throw new RuntimeException(throwable);
		}
	}

	protected DocumentBuilder createDocumentBuilder() throws ParserConfigurationException {
		return getDocumentBuilderFactory().newDocumentBuilder();
	}

	protected abstract Transformer createTransformer() throws TransformerConfigurationException;
}
