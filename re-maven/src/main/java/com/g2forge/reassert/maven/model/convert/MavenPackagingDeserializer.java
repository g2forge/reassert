package com.g2forge.reassert.maven.model.convert;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.gearbox.maven.packaging.IMavenPackaging;
import com.g2forge.gearbox.maven.packaging.MavenPackaging;
import com.g2forge.gearbox.maven.packaging.UnknownMavenPackaging;

public class MavenPackagingDeserializer extends StdDeserializer<IMavenPackaging> {
	private static final long serialVersionUID = 7672345770783504031L;

	protected MavenPackagingDeserializer() {
		super(MavenPackaging.class);
	}

	@Override
	public IMavenPackaging deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		final JsonToken token = parser.currentToken();
		if (token != JsonToken.VALUE_STRING) context.handleUnexpectedToken(MavenPackaging.class, parser);
		final String text = parser.getText().trim();
		try {
			return HEnum.valueOf(MavenPackaging.class, Object::toString, true, String::toLowerCase, text);
		} catch (IllegalArgumentException exception) {
			return new UnknownMavenPackaging(text);
		}
	}
}