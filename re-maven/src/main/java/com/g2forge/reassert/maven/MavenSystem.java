package com.g2forge.reassert.maven;

import java.io.ByteArrayInputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLResolver;
import javax.xml.stream.XMLStreamException;

import com.ctc.wstx.api.WstxInputProperties;
import com.ctc.wstx.stax.WstxInputFactory;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.dataformat.xml.XmlFactory;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.gearbox.maven.MavenPackaging;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.maven.model.convert.MavenXmlModule;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class MavenSystem implements ISystem<MavenCoordinates> {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	protected final IContext context;

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final XmlMapper mapper = computeMapper();

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PUBLIC)
	private final MavenRepository repository = new MavenRepository(this);

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PUBLIC)
	private final MavenScanner scanner = new MavenScanner(this);

	protected XmlMapper computeMapper() {
		final XMLInputFactory xmlInputFactory = new WstxInputFactory();
		xmlInputFactory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, Boolean.FALSE);
		xmlInputFactory.setProperty(XMLInputFactory.SUPPORT_DTD, Boolean.FALSE);
		xmlInputFactory.setProperty(WstxInputProperties.P_UNDECLARED_ENTITY_RESOLVER, new XMLResolver() {
			@Override
			public Object resolveEntity(String publicID, String systemID, String baseURI, String namespace) throws XMLStreamException {
				return new ByteArrayInputStream(new byte[0]);
			}
		});

		final XmlMapper mapper = new XmlMapper(new XmlFactory(xmlInputFactory, null));
		mapper.registerModule(new ParanamerModule());
		mapper.registerModule(new MavenXmlModule());
		mapper.enable(MapperFeature.ACCEPT_CASE_INSENSITIVE_ENUMS);
		return mapper;
	}

	@Override
	public IDescriber<MavenCoordinates> getCoordinateDescriber() {
		return MavenCoordinatesDescriber.create();
	}

	public MavenCoordinates parse(String string) {
		final Matcher matcher = Pattern.compile("([^:]+):([^:]+):([^:]+)(:([^:]+))?").matcher(string);
		if (!matcher.matches()) throw new IllegalArgumentException();
		final String packagingGroup = matcher.group(4);
		final MavenPackaging packaging = ((packagingGroup != null) && (packagingGroup.length() > 0)) ? HEnum.valueOf(MavenPackaging.class, Object::toString, true, String::toLowerCase, matcher.group(5)) : MavenPackaging.JAR;
		return new MavenCoordinates(this, matcher.group(1), matcher.group(2), matcher.group(3), packaging);
	}

	@Override
	public MavenCoordinates withSystem(MavenCoordinates coordinates) {
		if (!isValid(coordinates)) throw new IllegalArgumentException();
		return coordinates.toBuilder().system(this).build();
	}
}
