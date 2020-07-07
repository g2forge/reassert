package com.g2forge.reassert.maven.model.convert;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.g2forge.alexandria.java.core.enums.HEnum;
import com.g2forge.reassert.maven.model.MavenPackaging;

public class MavenPackagingDeserializer extends StdDeserializer<MavenPackaging> {
	private static final long serialVersionUID = 7672345770783504031L;

	protected MavenPackagingDeserializer() {
		super(MavenPackaging.class);
	}

	@Override
	public MavenPackaging deserialize(JsonParser parser, DeserializationContext context) throws IOException, JsonProcessingException {
		final JsonToken token = parser.currentToken();
		if (token != JsonToken.VALUE_STRING) context.handleUnexpectedToken(MavenPackaging.class, parser);
		return HEnum.valueOf(MavenPackaging.class, Object::toString, String::toLowerCase, parser.getText().trim());
	}
}