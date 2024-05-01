package com.g2forge.reassert.maven;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLResolver;
import javax.xml.stream.XMLStreamException;

import com.ctc.wstx.api.WstxInputProperties;
import com.ctc.wstx.stax.WstxInputFactory;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.MapperFeature;
import com.fasterxml.jackson.dataformat.xml.XmlFactory;
import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.dataaccess.IDataSource;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.gearbox.maven.MavenPackaging;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.maven.model.MavenEffectivePOM;
import com.g2forge.reassert.maven.model.MavenPOM;
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

		final XmlMapper.Builder builder = XmlMapper.builder(new XmlFactory(xmlInputFactory, null));
		builder.addModule(new ParanamerModule());
		builder.addModule(new MavenXmlModule());
		builder.enable(MapperFeature.ACCEPT_CASE_INSENSITIVE_ENUMS);
		return builder.build();
	}

	@Override
	public IDescriber<MavenCoordinates> getCoordinateDescriber() {
		return MavenCoordinatesDescriber.create();
	}

	protected <T> T parse(IDataSource source, Class<T> type) {
		final XmlMapper mapper = getMapper();
		try (final InputStream stream = source.getStream(ITypeRef.of(InputStream.class))) {
			return mapper.readValue(stream, type);
		} catch (IOException e) {
			throw new RuntimeIOException("Failed to parse " + type.getSimpleName() + " from " + source, e);
		}
	}

	public MavenCoordinates parse(String string) {
		final Matcher matcher = Pattern.compile("([^:]+):([^:]+):([^:]+)(:([^:]+))?").matcher(string);
		if (!matcher.matches()) throw new IllegalArgumentException();
		final String packagingGroup = matcher.group(4);
		final MavenPackaging packaging = ((packagingGroup != null) && (packagingGroup.length() > 0)) ? HEnum.valueOf(MavenPackaging.class, Object::toString, true, String::toLowerCase, matcher.group(5)) : MavenPackaging.JAR;
		return new MavenCoordinates(this, matcher.group(1), matcher.group(2), matcher.group(3), packaging);
	}

	public MavenEffectivePOM parseEffectivePOM(IDataSource source) {
		return parse(source, MavenEffectivePOM.class);
	}

	public MavenPOM parsePOM(IDataSource source) {
		return parse(source, MavenPOM.class);
	}

	@Override
	public MavenCoordinates withSystem(MavenCoordinates coordinates) {
		if (!isValid(coordinates)) throw new IllegalArgumentException();
		return coordinates.toBuilder().system(this).build();
	}
}
