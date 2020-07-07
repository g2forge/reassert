package com.g2forge.reassert.maven.modifier;

import java.io.InputStream;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.stream.StreamSource;

import com.g2forge.alexandria.java.core.helpers.HBinary;
import com.g2forge.alexandria.java.function.IThrowFunction1;
import com.g2forge.alexandria.java.io.HIO;
import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.alexandria.java.type.ref.ITypeRef;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class XSLTMavenPOMModifier extends AXMLTransformMavenPOMModifier {
	protected final IDataSource xsl;

	@Override
	protected Transformer createTransformer() throws TransformerConfigurationException {
		return getTransformFactory().newTransformer(new StreamSource(getXsl().getStream(ITypeRef.of(InputStream.class))));
	}

	@Override
	public String getKey() {
		return HBinary.toHex(HIO.sha1(getXsl().getStream(ITypeRef.of(InputStream.class)), IThrowFunction1.identity()));
	}
}
