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
import com.g2forge.reassert.reassert.summary.model.RiskSummary;

import lombok.Builder;
import lombok.Data;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
public class RiskSummarySerializer extends StdSerializer<RiskSummary> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	public static class StoredRiskSummary {
		protected final Level level;

		protected final ICoordinates artifact;

		protected final String message;

		protected final IUsage usage;

		protected final ILicense license;

		protected final IFinding details;
	}

	private static final long serialVersionUID = -6909941323933353363L;

	protected final IRenderer<? super IFinding> renderer;

	protected RiskSummarySerializer(IRenderer<? super IFinding> renderer) {
		super(RiskSummary.class);
		this.renderer = renderer;
	}

	@Override
	public void serialize(RiskSummary value, JsonGenerator generator, SerializerProvider provider) throws IOException {
		final String message = renderer.render(value.getRisk());
		final IFinding risk = value.getRisk();
		final StoredRiskSummary stored = new StoredRiskSummary(risk.getLevel(), value.getArtifact(), message, value.getUsage(), value.getLicense(), risk);
		generator.getCodec().writeValue(generator, stored);
	}
}