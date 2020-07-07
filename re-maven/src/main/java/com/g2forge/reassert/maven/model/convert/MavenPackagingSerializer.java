package com.g2forge.reassert.maven.model.convert;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.reassert.maven.model.MavenPackaging;

public class MavenPackagingSerializer extends StdSerializer<MavenPackaging> {
	private static final long serialVersionUID = -3492690193271746495L;

	public MavenPackagingSerializer() {
		super(MavenPackaging.class);
	}

	public void serialize(MavenPackaging packaging, JsonGenerator generator, SerializerProvider provider) throws IOException, JsonProcessingException {
		generator.writeString(packaging.toString().toLowerCase());
	}
}