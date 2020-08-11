package com.g2forge.reassert.reassert.summary.convert;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.enigma.backend.convert.IRenderer;
import com.g2forge.reassert.core.model.report.IFinding;

import lombok.Getter;

@Getter
public class FindingSerializer extends StdSerializer<IFinding> {
	private static final long serialVersionUID = -1040908189768000201L;

	protected final IRenderer<? super IFinding> renderer;

	protected FindingSerializer(IRenderer<? super IFinding> renderer) {
		super(IFinding.class);
		this.renderer = renderer;
	}

	@Override
	public void serialize(IFinding value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		generator.getCodec().writeValue(generator, getRenderer().render(value));
	}
}