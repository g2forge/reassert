package com.g2forge.reassert.reassert.summary.convert;

import java.io.IOException;

import org.slf4j.event.Level;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.std.StdSerializer;
import com.g2forge.enigma.backend.convert.IRenderer;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.reassert.summary.model.FindingSummary;

import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
public class FindingSummarySerializer extends StdSerializer<FindingSummary> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class StoredFindingSummary {
		protected final Level level;

		protected final ICoordinates artifact;

		protected final String message;

		protected final IUsage usage;

		protected final ILicense license;

		protected final IFinding details;
	}

	private static final long serialVersionUID = -6909941323933353363L;

	protected final IRenderer<? super IFinding> renderer;

	protected FindingSummarySerializer(IRenderer<? super IFinding> renderer) {
		super(FindingSummary.class);
		this.renderer = renderer;
	}

	@Override
	public void serialize(FindingSummary value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		final IFinding finding = value.getFinding();
		final String message = renderer.render(finding);
		final StoredFindingSummary stored = new StoredFindingSummary(finding.getLevel(), value.getArtifact(), message, value.getUsage(), value.getLicense(), finding);
		generator.getCodec().writeValue(generator, stored);
	}
}